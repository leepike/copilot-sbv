--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.SBV.Common
  ( mkTmpStVar
  , mkUpdateStFn
  , mkQueueVar
  , mkQueuePtrVar
  , mkExtTmpVar
  , mkObserverFn
  , mkTriggerGuardFn
  , mkTriggerArgFn
  , mkTriggerArgIdx
  ) where

import Copilot.Core (Id)
import Prelude hiding (id)

mkVar :: String -> Id -> String
mkVar str id = str ++ show id

mkTmpStVar :: Id -> String
mkTmpStVar = mkVar "tmp_"

mkUpdateStFn :: Id -> String
mkUpdateStFn = mkVar "update_state_" 

mkQueueVar :: Id -> String
mkQueueVar = mkVar "queue_" 

mkQueuePtrVar :: Id -> String
mkQueuePtrVar = mkVar "ptr_" 

mkExtTmpVar :: String -> String
mkExtTmpVar = ("ext_" ++)

mkObserverFn :: String -> String
mkObserverFn = ("observer_" ++)

mkTriggerGuardFn :: String -> String
mkTriggerGuardFn = ("trigger_guard_" ++)

mkTriggerArgFn :: Int -> String -> String
mkTriggerArgFn i nm = "trigger_" ++ nm ++ "_arg_" ++ show i

mkTriggerArgIdx :: [a] -> [(Int, a)]
mkTriggerArgIdx args = zip [0,1 ..] args
