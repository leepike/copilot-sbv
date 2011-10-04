--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Copilot.Compile.SBV.MetaTable
  ( StreamInfo (..)
  , StreamInfoMap
  , ExternInfoMap
  , ExternFunInfo (..)
  , ExternFunInfoMap
  , TriggerInfo (..)
  , TriggerInfoMap
  , ObserverInfo (..)
  , ObserverInfoMap
  , MetaTable (..)
  , allocMetaTable
  , argToCall
  , Arg(..)
  , c2Args
  ) where

import Copilot.Compile.SBV.Common
import qualified Copilot.Core as C
--import qualified Copilot.Core.External as C (ExternVar (..), externVars)

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

type ExternInfoMap = Map C.Name C.UType

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
  , externInfoMap     :: ExternInfoMap
  , externFunInfoMap  :: ExternFunInfoMap
  , triggerInfoMap    :: TriggerInfoMap
  , observerInfoMap   :: ObserverInfoMap }

--------------------------------------------------------------------------------

allocMetaTable :: C.Spec -> MetaTable
allocMetaTable spec =
  let
    streamInfoMap_   = M.fromList $ map allocStream (C.specStreams spec)
    externInfoMap_   = M.fromList $ map allocExtern (C.externVars spec)
    triggerInfoMap_  = M.fromList $ map allocTrigger (C.specTriggers spec)
    observerInfoMap_ = M.fromList $ map allocObserver (C.specObservers spec)
  in
    MetaTable
      streamInfoMap_
      externInfoMap_
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

allocExtern :: C.ExtVar -> (C.Name, C.UType)
allocExtern (C.ExtVar name t) =
  (name, t)

--------------------------------------------------------------------------------

allocTrigger :: C.Trigger -> (C.Name, TriggerInfo)
allocTrigger C.Trigger { C.triggerName  = name
                       , C.triggerGuard = guard
                       , C.triggerArgs  = args } = 
  let mkArgArgs :: C.UExpr -> [String]
      mkArgArgs C.UExpr { C.uExprExpr = e } = 
        nub (concatMap argToCall (c2Args e)) in
  let triggerInfo = 
        TriggerInfo { guardArgs      = nub (concatMap argToCall (c2Args guard))
                    , triggerArgArgs = map mkArgArgs args } in
  (name, triggerInfo)

--------------------------------------------------------------------------------

allocObserver :: C.Observer -> (C.Name, ObserverInfo)
allocObserver C.Observer { C.observerName = name
                         , C.observerExpr = e } =
  let
    observerInfo =
      ObserverInfo { observerArgs = nub (concatMap argToCall (c2Args e)) }
  in
    (name, observerInfo)

--------------------------------------------------------------------------------
-- Getting SBV function args from the expressions.

c2Args :: C.Expr a -> [Arg]
c2Args e = nub $ c2Args_ e

-- Kinds of arguments to SBV functions
data Arg = Extern    C.Name
         | ExternFun C.Name
         | ExternArr C.Name
         | Queue     C.Id
  deriving Eq

argToCall :: Arg -> [String]
argToCall (Extern name) = [mkExtTmpVar name]
argToCall (Queue id ) = [ mkQueueVar id 
                        , mkQueuePtrVar id ]

-- Gathers the names of the arguments to the SBV updateState function so that we
-- can construct the prototypes.

-- XXX It depends on gathering the the arguments in the same order that SBV uses
-- them.  SBV makes the arguments in the order that the cgInput and cgInputArr
-- are pushed into the SBVCodeGen.  However, there should really be an API for
-- getting the prototypes.

c2Args_ :: C.Expr a -> [Arg]
c2Args_ e0 = case e0 of
  C.Const _ _ -> [] 

  C.Drop _ _ id -> [ Queue id ]
 
  C.Local _ _ _ e1 e2 -> c2Args_ e1 ++ c2Args_ e2

  C.Var _ _ -> []

  C.ExternVar   _ name -> [Extern name]

  C.ExternFun   _ name args _ -> 
    ExternFun name : concatMap (\C.UExpr { C.uExprExpr = expr } 
                                             -> c2Args expr) 
                                        args

  C.ExternArray _ _ name idx _ -> ExternArr name : c2Args_ idx

  C.Op1 _ e -> c2Args_ e

  C.Op2 _ e1 e2 -> c2Args_ e1 ++ c2Args_ e2

  C.Op3 _ e1 e2 e3 -> c2Args_ e1 ++ c2Args_ e2 ++ c2Args_ e3
