--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Implements queues holding stream values.

module Copilot.Compile.SBV.Queue
  ( Queue
  , dropFirstElemAndSnoc
  , lookahead
  -- , size
  , queue
  ) where

import qualified Data.SBV as S
import qualified Data.SBV.Internals as S
import Copilot.Compile.SBV.Common

type QueueSize = S.SWord8

data Queue a = Queue
  { queueRingBuffer :: Var a -- Pointer to the queue
  , queuePointer    :: Var a -- Index into the queue
  , size            :: Int   -- Size of the queue
  }

-- XXX Broken
-- The purpose of this is to update queue[p] with x and update x += 1.  Since we
-- can't do assignments like this in SBV (it's symbolic, so we don't know the
-- value of the index p, which explodes into a huge expression), we have to do
-- the assignment
dropFirstElemAndSnoc :: S.SBV a -> Queue a -> S.SBVCodeGen ()
dropFirstElemAndSnoc x (Queue buf p sz) =
--  updateQueue >> 
    updatePointer
    
  where

  -- updateQueue :: S.SBVCodeGen ()
  -- updateQueue = do
  --   pin   <- S.cgInput p
  --   bufin <- S.cgInputArr sz buf 
  --   S.cgOutput buf (S.select (bufin :: [QueueSize]) 0 (pin :: QueueSize))

  updatePointer :: S.SBVCodeGen ()
  updatePointer = do 
    pin <- S.cgInput p
    let nextPin = pin + 1 :: QueueSize
    S.cgOutput p $ S.ite (nextPin S..>= fromIntegral sz) 0 nextPin


-- The function generated takes two inputs: (1) a buffer buffin and (2) a pointer
lookahead :: Int -> Queue a -> S.SBVCodeGen ()
lookahead i (Queue buf p sz) = do
  pin   <- S.cgInput p
  bufin <- S.cgInputArr sz buf 
  let k = ((pin :: QueueSize) + fromIntegral i) `S.pMod` fromIntegral sz
  let elem = S.select (bufin :: [QueueSize]) 0 k
  S.cgOutput undefined elem


queue :: String -> [a] -> Queue a
queue name xs =
  Queue
    { queueRingBuffer = "queue_buffer_" ++ name
    , queuePointer    = "queue_pointer_" ++ name
    , size            = fromIntegral (length xs) }
