--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Implements queues holding stream values.

module Copilot.Compile.SBV.Queue
  ( Queue(..)
  , lookahead
  , QueueSize
  ) where

import Prelude hiding (id, rem)
import qualified Data.SBV as S

import Copilot.Core.Expr (DropIdx)
import Copilot.Core.Error (impossible)

--------------------------------------------------------------------------------

type QueueSize = DropIdx

data Queue a = Queue
  { queue :: [a] }

--------------------------------------------------------------------------------

lookahead :: (S.SymWord a) => DropIdx -> [S.SBV a] -> S.SBV QueueSize -> S.SBV a
lookahead i buf ptr = 
  let sz = fromIntegral $ length buf in
  let (_, rem) = (ptr + fromIntegral i) `S.sQuotRem` sz in
  let defaultVal = if null buf 
                     then impossible "lookahead" "copilot-sbv"
                     else head buf                    in
  S.select buf defaultVal rem

--------------------------------------------------------------------------------
