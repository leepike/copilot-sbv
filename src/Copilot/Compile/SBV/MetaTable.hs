--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Copilot.Compile.SBV.MetaTable
  ( StreamInfo (..)
  , ExternInfo (..)
  , StreamInfoMap
  , ExternInfoMap
  , MetaTable (..)
  , allocMetaTable
  ) where

import qualified Data.SBV as S

import Copilot.Compile.SBV.Common
import qualified Copilot.Compile.SBV.Queue as Q
import qualified Copilot.Compile.SBV.Witness as W
import qualified Copilot.Core as C
import Copilot.Core.Spec.Externals (Extern (..), externals)

import Control.Monad (liftM)
import Data.Map (Map)
import qualified Data.Map as M
import Prelude hiding (id)

--------------------------------------------------------------------------------

data StreamInfo = forall a . StreamInfo
  { streamInfoQueue   :: Q.Queue a
  , streamInfoTempVar :: Var a
  , streamInfoType    :: C.Type a }

type StreamInfoMap = Map C.Id StreamInfo

--------------------------------------------------------------------------------

data ExternInfo = forall a . ExternInfo
  { externInfoVar     :: Var a
  , externInfoType    :: C.Type a }

type ExternInfoMap = Map C.Name ExternInfo

--------------------------------------------------------------------------------

data MetaTable = MetaTable
  { streamInfoMap     :: StreamInfoMap
  , externInfoMap     :: ExternInfoMap }

--------------------------------------------------------------------------------

allocMetaTable :: C.Spec -> MetaTable
allocMetaTable spec =
  let streamInfoMap_ = M.fromList $ map allocStream (C.specStreams spec) in
  let externInfoMap_ = M.fromList $ map allocExtern (externals spec) in
  MetaTable streamInfoMap_ externInfoMap_

--------------------------------------------------------------------------------

allocStream :: C.Stream -> (C.Id, StreamInfo)
allocStream
  C.Stream
    { C.streamId       = id
    , C.streamBuffer   = buf
    , C.streamExprType = t
    } =
    let que = Q.queue (mkQueueName id) buf in
    let tmp = var (mkTempVarName id) (C.uninitialized t) in
    let
      strmInfo =
        StreamInfo
          { streamInfoQueue       = que
          , streamInfoTempVar     = tmp
          , streamInfoType        = t } in
    (id, strmInfo)

--------------------------------------------------------------------------------

allocExtern :: Extern -> (C.Name, ExternInfo)
allocExtern (Extern name t) =
  let v = var (mkExternName name) (C.uninitialized t) in
  (name, ExternInfo v t)

-- --------------------------------------------------------------------------------

mkExternName :: C.Name -> String
mkExternName name = "ext_" ++ name

mkQueueName :: C.Id -> String
mkQueueName id = "str" ++ show id

mkTempVarName :: C.Id -> String
mkTempVarName id = "tmp" ++ show id

-- --------------------------------------------------------------------------------
