--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Implements queues holding stream values.

module Copilot.Compile.SBV.Queue
  ( Queue(..)
  , lookahead
  , QueueSize
  ) where

import Prelude hiding (id)
import qualified Data.SBV as S
import qualified Data.SBV.Internals as S

import Copilot.Core.Expr (DropIdx)

--------------------------------------------------------------------------------

type QueueSize = DropIdx

data Queue a = Queue
  { queue :: [a] }

--------------------------------------------------------------------------------

lookahead :: (S.HasSignAndSize a, S.SymWord a) 
          => DropIdx -> [S.SBV a] -> S.SBV QueueSize -> S.SBV a
lookahead i buf ptr = 
  let sz = fromIntegral $ length buf in
  let k = ptr + fromIntegral (i `mod` sz) `S.pMod` fromIntegral sz in
  let defaultVal = if null buf then error "lookahead error" else head buf in
  S.select buf defaultVal k

--------------------------------------------------------------------------------
