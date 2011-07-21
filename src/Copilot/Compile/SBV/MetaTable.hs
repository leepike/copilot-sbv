--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Copilot.Compile.SBV.MetaTable
  ( StreamInfo (..)
  , StreamInfoMap
  , ExternInfo (..)
  , ExternInfoMap
  , TriggerInfo (..)
  , TriggerInfoMap
  , MetaTable (..)
  , allocMetaTable
  ) where

import qualified Data.SBV as S
import qualified Data.SBV.Internals as S

import Copilot.Compile.SBV.Common
import qualified Copilot.Compile.SBV.Queue as Q
import qualified Copilot.Compile.SBV.Witness as W

import qualified Copilot.Core as C
import Copilot.Core.Type.Equality ((=~=), coerce, cong)
import qualified Copilot.Core.Spec.Externals as C (Extern (..), externals) 

import Data.Map (Map)
import Data.List (nub)
import qualified Data.Map as M
import Prelude hiding (id)

--------------------------------------------------------------------------------

data StreamInfo = forall a . StreamInfo
  { streamInfoQueue   :: [a]
  , streamFnInputs    :: [String]
  , streamInfoType    :: C.Type a }

type StreamInfoMap = Map C.Id StreamInfo

--------------------------------------------------------------------------------

data ExternInfo = forall a . ExternInfo
  { --extern            :: String
    externInfoType    :: C.Type a }

type ExternInfoMap = Map C.Name ExternInfo

--------------------------------------------------------------------------------

data TriggerInfo = TriggerInfo
  { guardArgs      :: [String]
  , triggerArgArgs :: [[String]] }

type TriggerInfoMap = Map C.Name TriggerInfo

--------------------------------------------------------------------------------

data MetaTable = MetaTable
  { streamInfoMap     :: StreamInfoMap
  , externInfoMap     :: ExternInfoMap 
  , triggerInfoMap    :: TriggerInfoMap }

--------------------------------------------------------------------------------

allocMetaTable :: C.Spec -> MetaTable
allocMetaTable spec =
  let streamInfoMap_  = M.fromList $ map allocStream (C.specStreams spec) in
  let externInfoMap_  = M.fromList $ map allocExtern (C.externals spec) in
  let triggerInfoMap_ = M.fromList $ map allocTrigger (C.specTriggers spec) in
  MetaTable streamInfoMap_ externInfoMap_ triggerInfoMap_

--------------------------------------------------------------------------------

allocStream :: C.Stream -> (C.Id, StreamInfo)
allocStream C.Stream
              { C.streamId       = id
              , C.streamBuffer   = buf
              , C.streamExpr     = e
              , C.streamExprType = t
              } =
--  let que = Q.queue t id buf in
  let
    strmInfo =
      StreamInfo
        { streamInfoQueue       = buf
        , streamFnInputs        = nub (c2Args e)
        , streamInfoType        = t } in
  (id, strmInfo)

--------------------------------------------------------------------------------

allocExtern :: C.Extern -> (C.Name, ExternInfo)
allocExtern (C.Extern name t) =
  -- let cgVar = do W.SymWordInst <- return (W.symWordInst t)
  --                W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
  --                Just p <- return (t =~= t)
  --                input <- S.cgInput (mkExtTmpVar name)
  --                return $ coerce (cong p) input in
  (name, ExternInfo t)

--------------------------------------------------------------------------------

allocTrigger :: C.Trigger -> (C.Name, TriggerInfo)
allocTrigger C.Trigger { C.triggerName  = name
                       , C.triggerGuard = guard
                       , C.triggerArgs  = args } = 
  let mkArgArgs :: C.TriggerArg -> [String]
      mkArgArgs C.TriggerArg { C.triggerArgExpr = e } = nub (c2Args e) in
  let triggerInfo = 
        TriggerInfo { guardArgs      = nub (c2Args guard)
                    , triggerArgArgs = map mkArgArgs args } in
  (name, triggerInfo)

--------------------------------------------------------------------------------
-- Getting SBV function args from the expressions.

newtype C2Args a = C2Args
  { c2Args :: [String] }

-- Gathers the names of the arguments to the SBV updateState function so that we
-- can construct the prototypes.

-- XXX It depends on gathering the the arguments in the same order that SBV uses
-- them.  SBV makes the arguments in the order that the cgInput and cgInputArr
-- are pushed into the SBVCodeGen.  However, there should really be an API for
-- getting the prototypes.
instance C.Expr C2Args where
  const _ _ = C2Args [] 

  drop _ _ id = C2Args [ mkQueuePtrVar id
                       , mkQueueVar id ]
 
  local _ _ _ e1 e2 = 
    C2Args $ c2Args e1 ++ c2Args e2

  var _ _ = C2Args []

  extern _ name = C2Args [mkExtTmpVar name]

  op1 _ e = C2Args (c2Args e)

  op2 _ e1 e2 = 
    C2Args $ c2Args e1 ++ c2Args e2

  op3 _ e1 e2 e3 = 
    C2Args $ c2Args e1 ++ c2Args e2 ++ c2Args e3
