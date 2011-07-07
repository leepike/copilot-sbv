--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.SBV.Queue
  ( Queue
  , dropFirstElemAndSnoc
  , lookahead
  -- , size
  , queue
  ) where

import qualified Data.SBV as S
import Copilot.Compile.SBV.Common

type QueueSize = S.SWord8

data Queue a = Queue
  { queueRingBuffer :: Var a 
  , queuePointer    :: Var a
  , size            :: Int
  }

-- Returns a new element and updates the pointer.  Put the value into the array
-- in C.
dropFirstElemAndSnoc :: S.SBV a -> Queue a -> S.SBVCodeGen ()
dropFirstElemAndSnoc x
  Queue
    { queueRingBuffer = buf
    , queuePointer    = p
    , size            = sz
    } =
  updateQueue >>
    updatePointer
  where
  updateQueue :: S.SBVCodeGen ()
  updateQueue = do
    pin   <- S.cgInput p
    bufin <- S.cgInputArr sz buf 
    S.cgOutput buf (S.select (bufin :: [QueueSize]) 0 (pin :: QueueSize))

  updatePointer :: S.SBVCodeGen ()
  updatePointer = do 
    pin <- S.cgInput p
    let nextPin = pin + 1 :: QueueSize
    S.cgOutput p $ S.ite (nextPin S..>= fromIntegral sz) 0 nextPin


-- Returns lookahead
lookahead :: Int -> Queue a -> S.SBVCodeGen ()
lookahead i
  Queue
    { queueRingBuffer = buf
    , queuePointer    = p
    , size            = sz
    } = do
  pin   <- S.cgInput p
  bufin <- S.cgInputArr sz buf 
  let k = ((pin :: QueueSize) + fromIntegral i) `S.pMod` fromIntegral sz
  let elem = S.select (bufin :: [QueueSize]) 0 k
  S.cgReturn elem


queue :: String -> [a] -> Queue a
queue name xs =
  Queue
    { queueRingBuffer = "queue_buffer_" ++ name
    , queuePointer    = "queue_pointer_" ++ name
    , size            = fromIntegral (length xs) }
