--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Implements queues holding stream values.

module Copilot.Compile.SBV.Queue
  ( Queue
--  , dropFirstElemAndSnoc
  , lookahead
  -- , size
  , queue
  ) where

import qualified Data.SBV as S
import qualified Data.SBV.Internals as S

import qualified Copilot.Compile.SBV.Witness as W

import Copilot.Core.Expr (DropIdx)
import qualified Copilot.Core as C (Type)

--------------------------------------------------------------------------------
type QueueSize = S.SWord8

data Queue a = Queue
  { queueRingBuffer :: [S.SBV a] -- Pointer to the queue
  , queuePointer    :: QueueSize -- Index into the queue
  , size            :: Int       -- Size of the queue
  }

-- dropFirstElemAndSnoc :: Queue a -> S.SBVCodeGen ()
-- dropFirstElemAndSnoc (Queue _ p sz) =
--   updatePointer
--   where
--   updatePointer :: S.SBVCodeGen ()
--   updatePointer = do 
--     -- pin <- S.cgInput p
--     -- bufin <- S.cgInputArr sz buf 
--     let nextPin = p + 1 
--     S.cgOutput p $ S.ite (nextPin S..>= fromIntegral sz) 0 nextPin

--  updateQueue >> 
  -- updateQueue :: S.SBVCodeGen ()
  -- updateQueue = do
  --   pin   <- S.cgInput p
  --   bufin <- S.cgInputArr sz buf 
  --   S.cgOutput buf (S.select (bufin :: [QueueSize]) 0 (pin :: QueueSize))

lookahead :: (S.HasSignAndSize a, S.SymWord a) 
          => DropIdx -> Queue a -> S.SBVCodeGen (S.SBV a)
lookahead i (Queue buf p sz) = do 
--  pin   <- S.cgInput p

  let k = (p + fromIntegral i) `S.pMod` fromIntegral sz
  let defaultVal = if null buf then error "lookahead error" else head buf
  return $ S.select buf defaultVal k


queue :: C.Type a -> String -> [a] -> S.SBVCodeGen (Queue a)
queue t name xs = do
  W.SymWordInst        <- return (W.symWordInst t)
  W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
--  S.cgOutputArr queueName (map S.literal xs)
--  S.cgOutput pointerName (0 :: QueueSize)
  let sz = fromIntegral (length xs)
  buf <- S.cgInputArr sz queueName
  p   <- S.cgInput pointerName
  return $ 
    Queue { queueRingBuffer = buf
          , queuePointer    = p
          , size            = sz }
  where
  queueName   = "queue_buffer_" ++ name
  pointerName = "queue_pointer_" ++ name
