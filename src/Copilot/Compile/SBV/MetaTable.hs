--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Copilot.Compile.SBV.MetaTable
  ( StreamInfo (..)
  , StreamInfoMap
  , ExternVarInfoMap
  , ExternArrInfoMap
  , ExternFunInfo (..)
  , ExternFunInfoMap
  , TriggerInfo (..)
  , TriggerInfoMap
  , ObserverInfo (..)
  , ObserverInfoMap
  , MetaTable (..)
  , allocMetaTable
  , collectArgs
  , Arg(..)
  , c2Args
  ) where

import Copilot.Compile.SBV.Common
import qualified Copilot.Core as C

import Data.Map (Map)
import Data.List (nub)
import qualified Data.Map as M
import Prelude hiding (id)

--------------------------------------------------------------------------------

data StreamInfo = forall a . StreamInfo
  { streamInfoQueue   :: [a]
  , streamInfoType    :: C.Type a }

type StreamInfoMap = Map C.Id StreamInfo

--------------------------------------------------------------------------------

type ExternVarInfoMap = Map C.Name C.ExtVar

--------------------------------------------------------------------------------

type ExternArrInfoMap = Map C.Name C.ExtArray

--------------------------------------------------------------------------------

data ExternFunInfo = forall a . ExternFunInfo
  { externFunInfoArgs :: [(C.UType, C.UExpr)]
  , externFunInfoType :: C.Type a }

type ExternFunInfoMap = Map C.Name ExternFunInfo

--------------------------------------------------------------------------------

data TriggerInfo = TriggerInfo
  { guardArgs      :: [String]
  , triggerArgArgs :: [[String]] }

type TriggerInfoMap = Map C.Name TriggerInfo

--------------------------------------------------------------------------------

data ObserverInfo = ObserverInfo
  { observerArgs :: [String] }

type ObserverInfoMap = Map C.Name ObserverInfo

--------------------------------------------------------------------------------

data MetaTable = MetaTable
  { streamInfoMap     :: StreamInfoMap
  , externVarInfoMap  :: ExternVarInfoMap
  , externArrInfoMap  :: ExternArrInfoMap
  , externFunInfoMap  :: ExternFunInfoMap
  , triggerInfoMap    :: TriggerInfoMap
  , observerInfoMap   :: ObserverInfoMap }

--------------------------------------------------------------------------------

allocMetaTable :: C.Spec -> MetaTable
allocMetaTable spec =
  let
    streamInfoMap_    = M.fromList $ map allocStream     (C.specStreams spec)
    externVarInfoMap_ = M.fromList $ map allocExternVars (C.externVars spec)
    externArrInfoMap_ = M.fromList $ map allocExternArrs (C.externArrays spec)
    triggerInfoMap_   = M.fromList $ map allocTrigger    (C.specTriggers spec)
    observerInfoMap_  = M.fromList $ map allocObserver   (C.specObservers spec)
  in
    MetaTable
      streamInfoMap_
      externVarInfoMap_
      externArrInfoMap_
      (error "undefined in MetaTable.hs in copilot-sbv.")
      triggerInfoMap_
      observerInfoMap_

--------------------------------------------------------------------------------

allocStream :: C.Stream -> (C.Id, StreamInfo)
allocStream C.Stream
              { C.streamId       = id
              , C.streamBuffer   = buf
              , C.streamExprType = t
              } =
  let
    strmInfo =
      StreamInfo
        { streamInfoQueue       = buf
        , streamInfoType        = t } in
  (id, strmInfo)

--------------------------------------------------------------------------------

allocExternVars :: C.ExtVar -> (C.Name, C.ExtVar)
allocExternVars var =
  (C.externVarName var, var)

--------------------------------------------------------------------------------

allocExternArrs :: C.ExtArray -> (C.Name, C.ExtArray)
allocExternArrs arr =
  (C.externArrayName arr, arr)

--------------------------------------------------------------------------------

allocTrigger :: C.Trigger -> (C.Name, TriggerInfo)
allocTrigger C.Trigger { C.triggerName  = name
                       , C.triggerGuard = guard
                       , C.triggerArgs  = args } 
  =
  let mkArgArgs :: C.UExpr -> [String]
      mkArgArgs C.UExpr { C.uExprExpr = e } = collectArgs e in
  let triggerInfo = 
        TriggerInfo { guardArgs      = collectArgs guard 
                    , triggerArgArgs = map mkArgArgs args } in
  (name, triggerInfo)

--------------------------------------------------------------------------------

allocObserver :: C.Observer -> (C.Name, ObserverInfo)
allocObserver C.Observer { C.observerName = name
                         , C.observerExpr = e } 
  = 
  let observerInfo = ObserverInfo { observerArgs = collectArgs e } in
  (name, observerInfo)

--------------------------------------------------------------------------------

-- Kinds of arguments to SBV functions
data Arg = Extern    C.Name
         | ExternFun C.Name
         | ExternArr C.Name
         | Queue     C.Id
  deriving Eq

argToCall :: Arg -> [String]
argToCall (Extern name)       = [mkExtTmpVar name]
argToCall (ExternArr name)    = [mkExtTmpVar name]
argToCall (Queue id )         = [ mkQueueVar id 
                                , mkQueuePtrVar id ]

--------------------------------------------------------------------------------

collectArgs :: C.Expr a -> [String]
collectArgs e = nub (concatMap argToCall (c2Args e))

--------------------------------------------------------------------------------

-- Gathers the names of the arguments to the SBV updateState function so that we
-- can construct the prototypes.

-- XXX It depends on gathering the the arguments in the same order that SBV uses
-- them.  SBV makes the arguments in the order that the cgInput and cgInputArr
-- are pushed into the SBVCodeGen.  However, there should really be an API for
-- getting the prototypes.

c2Args :: C.Expr a -> [Arg]
c2Args e = nub $ c2Args_ e

c2Args_ :: C.Expr a -> [Arg]
c2Args_ e0 = case e0 of
  C.Const _ _          -> [] 

  C.Drop _ _ id        -> [ Queue id ]
 
  C.Local _ _ _ e1 e2  -> c2Args_ e1 ++ c2Args_ e2

  C.Var _ _            -> []

  C.ExternVar   _ name -> [Extern name]

  C.ExternFun   _ name args _ -> 
    ExternFun name : concatMap (\C.UExpr { C.uExprExpr = expr } 
                                             -> c2Args expr) 
                                        args

  C.ExternArray _ _ name _ _  _ -> [ExternArr name] -- : c2Args_ idx

  C.Op1 _ e        -> c2Args_ e

  C.Op2 _ e1 e2    -> c2Args_ e1 ++ c2Args_ e2

  C.Op3 _ e1 e2 e3 -> c2Args_ e1 ++ c2Args_ e2 ++ c2Args_ e3

--------------------------------------------------------------------------------
