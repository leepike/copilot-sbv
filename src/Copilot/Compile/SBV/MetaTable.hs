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
import qualified Data.SBV.Internals as S

import Copilot.Compile.SBV.Common
import qualified Copilot.Compile.SBV.Queue as Q
import qualified Copilot.Compile.SBV.Witness as W

import qualified Copilot.Core as C
import Copilot.Core.Type.Equality ((=~=), coerce, cong)
import qualified Copilot.Core.Spec.Externals as C (Extern (..), externals) 

import Data.Map (Map)
import qualified Data.Map as M
import Prelude hiding (id)

--------------------------------------------------------------------------------

data StreamInfo = forall a . StreamInfo
  { streamInfoQueue   :: Q.Queue a
--  , streamId          :: C.Id
  , streamInfoType    :: C.Type a }

type StreamInfoMap = Map C.Id StreamInfo

--------------------------------------------------------------------------------

data ExternInfo = forall a . ExternInfo
  { externInfoVar     :: Var a
  , externInfoSBV     :: S.SBVCodeGen (S.SBV a)
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
  let externInfoMap_ = M.fromList $ map allocExtern (C.externals spec) in
  MetaTable streamInfoMap_ externInfoMap_

--------------------------------------------------------------------------------

allocStream :: C.Stream -> (C.Id, StreamInfo)
allocStream
  C.Stream
    { C.streamId       = id
    , C.streamBuffer   = buf
    , C.streamExprType = t
    } =
    let que = Q.queue t (mkQueueVar (show id)) buf in
--    let tmp = mkTmpStVar (show id)  in
    let
      strmInfo =
        StreamInfo
          { streamInfoQueue       = que
--          , streamId              = id
          , streamInfoType        = t } in
    (id, strmInfo)

--------------------------------------------------------------------------------

allocExtern :: C.Extern -> (C.Name, ExternInfo)
allocExtern (C.Extern name t) =
  let v     = var name (C.uninitialized t) in
  let cgVar = do W.SymWordInst <- return (W.symWordInst t)
                 W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
                 Just p <- return (t =~= t)
                 input <- S.cgInput v
                 return $ coerce (cong p) input in
  (name, ExternInfo v cgVar t)

-- --------------------------------------------------------------------------------

-- mkExternName :: C.Name -> String
-- mkExternName name = "ext_" ++ name

-- mkQueueName :: C.Id -> String
-- mkQueueName id = show id

-- mkTempVarName :: C.Id -> String
-- mkTempVarName id = "tmp" ++ show id

-- --------------------------------------------------------------------------------
