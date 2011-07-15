--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.SBV.Common
  ( --Var
--  , var
  mkTmpStVar
  , mkUpdateStFunc
  , mkQueueVar
  , mkQueuePtrVar
  , mkExtTmpVar
  ) where

--type Var a = String

-- XXX put all mkvar names in one place and carry them around in MetaTable?

-- var :: String -> a -> Var a
-- var name _ = name 

mkTmpStVar :: String -> String
mkTmpStVar = ("tmp_" ++)

mkUpdateStFunc :: String -> String
mkUpdateStFunc = ("update_state_" ++)

mkQueueVar :: String -> String
mkQueueVar = ("queue_" ++)

mkQueuePtrVar :: String -> String
mkQueuePtrVar = ("ptr_" ++)

mkExtTmpVar :: String -> String
mkExtTmpVar = ("ext_" ++)
