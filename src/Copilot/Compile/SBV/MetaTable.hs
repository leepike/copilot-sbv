--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Copilot.Compile.SBV.MetaTable
  ( StreamInfo (..)
  , StreamInfoMap
  , ExternInfoMap
  , ExternFunInfo (..)
  , ExternFunInfoMap
  , TriggerInfo (..)
  , TriggerInfoMap
  , MetaTable (..)
  , allocMetaTable
  , argToCall
  , Arg(..)
  , c2Args
  ) where

import Copilot.Compile.SBV.Common
import qualified Copilot.Core as C
import qualified Copilot.Core.External as C (ExternVar (..), externVars)

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

data MetaTable = MetaTable
  { streamInfoMap     :: StreamInfoMap
  , externInfoMap     :: ExternInfoMap
  , externFunInfoMap  :: ExternFunInfoMap
  , triggerInfoMap    :: TriggerInfoMap }

--------------------------------------------------------------------------------

allocMetaTable :: C.Spec -> MetaTable
allocMetaTable spec =
  let streamInfoMap_  = M.fromList $ map allocStream (C.specStreams spec) in
  let externInfoMap_  = M.fromList $ map allocExtern (C.externVars spec) in
  let triggerInfoMap_ = M.fromList $ map allocTrigger (C.specTriggers spec) in
  MetaTable streamInfoMap_ externInfoMap_ undefined triggerInfoMap_

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

allocExtern :: C.ExternVar -> (C.Name, C.UType)
allocExtern (C.ExternVar name t) =
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
-- Getting SBV function args from the expressions.

c2Args :: C2Args a -> [Arg]
c2Args e = nub $ c2Args_ e

newtype C2Args a = C2Args
  { c2Args_ :: [Arg] }

-- Kinds of arguments to SBV functions
data Arg = Extern    C.Name
         | ExternFun C.Name
         | ExternArr C.Name
         | Queue     C.Id
  deriving Eq

argToCall :: Arg -> [String]
argToCall (Extern name) = [mkExtTmpVar name]
argToCall (Queue id ) = [ mkQueuePtrVar id
                        , mkQueueVar id ]

-- Gathers the names of the arguments to the SBV updateState function so that we
-- can construct the prototypes.

-- XXX It depends on gathering the the arguments in the same order that SBV uses
-- them.  SBV makes the arguments in the order that the cgInput and cgInputArr
-- are pushed into the SBVCodeGen.  However, there should really be an API for
-- getting the prototypes.
instance C.Expr C2Args where
  const _ _ = C2Args [] 

  drop _ _ id = C2Args [ Queue id ]
 
  local _ _ _ e1 e2 = 
    C2Args $ c2Args_ e1 ++ c2Args_ e2

  var _ _ = C2Args []

  externVar   _ name = C2Args [Extern name]

  externFun   _ name args = 
    C2Args $ ExternFun name : concatMap (\C.UExpr { C.uExprExpr = exp } 
                                             -> c2Args exp) 
                                        args

  externArray _ _ name idx = C2Args $ ExternArr name : c2Args_ idx

  op1 _ e = C2Args (c2Args_ e)

  op2 _ e1 e2 = 
    C2Args $ c2Args_ e1 ++ c2Args_ e2

  op3 _ e1 e2 e3 = 
    C2Args $ c2Args_ e1 ++ c2Args_ e2 ++ c2Args_ e3
