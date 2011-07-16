--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.SBV.Common
  ( --Var
--  , var
  mkTmpStVar
  , mkUpdateStFn
  , mkQueueVar
  , mkQueuePtrVar
  , mkExtTmpVar
  , mkTriggerGuardFn
  , mkTriggerArgFn
  ) where

import Copilot.Core (Id)
import Prelude hiding (id)

--type Var a = String

-- XXX put all mkvar names in one place and carry them around in MetaTable?

-- var :: String -> a -> Var a
-- var name _ = name 

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

mkTriggerGuardFn :: String -> String
mkTriggerGuardFn = ("trigger_guard_" ++)

mkTriggerArgFn :: Int -> String -> String
mkTriggerArgFn i nm = "trigger_arg_" ++ nm ++ show i
