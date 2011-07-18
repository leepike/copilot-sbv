--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Implements queues holding stream values.

module Copilot.Compile.SBV.Queue
  ( Queue(..)
  , lookahead
  , queue
  , QueueSize
  ) where

import Prelude hiding (id)
import qualified Data.SBV as S
import qualified Data.SBV.Internals as S

import qualified Copilot.Compile.SBV.Witness as W
import Copilot.Compile.SBV.Common

import Copilot.Core.Expr (DropIdx)
import qualified Copilot.Core as C (Type, Id)
import Copilot.Core.Type.Equality ((=~=), coerce, cong)

--------------------------------------------------------------------------------

type QueueSize = S.SWord8

data Queue a = Queue
  { queueInits      :: [a]
  , queueRingBuffer :: S.SBVCodeGen [S.SBV a] -- Pointer to the queue
  , queuePointer    :: S.SBVCodeGen QueueSize -- Index into the queue
  , size            :: Int       -- Size of the queue
  }

--------------------------------------------------------------------------------

lookahead :: (S.HasSignAndSize a, S.SymWord a) 
          => DropIdx -> Queue a -> S.SBVCodeGen (S.SBV a)
lookahead i Queue { queueRingBuffer = sbvBuf
                  , queuePointer    =  ptr
                  , size            =  sz }   = do
  p <- ptr
  let k = (p + fromIntegral i) `S.pMod` fromIntegral sz 
  buf <- sbvBuf
  let defaultVal = if null buf then error "lookahead error" else head buf
  return $ S.select buf defaultVal k

--------------------------------------------------------------------------------

queue :: C.Type a -> C.Id -> [a] -> Queue a
queue t id xs = 
  let sz  = fromIntegral (length xs) in
  let p   = S.cgInput (mkQueuePtrVar id) in
  let buf = do W.SymWordInst        <- return (W.symWordInst t)
               W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
               arr <- S.cgInputArr sz (mkQueueVar id)
               Just p_ <- return (t =~= t)
               return $ map (coerce (cong p_)) arr
  in Queue { queueInits      = xs 
           , queueRingBuffer = buf
           , queuePointer    = p
           , size            = sz } 

--------------------------------------------------------------------------------
