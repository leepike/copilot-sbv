--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.SBV.Common
  ( Var
  , var
  , mkTmpStVar
  , mkUpdateStFunc
  , mkQueueVar
  , mkQueuePtrVar
  ) where

type Var a = String

var :: String -> a -> Var a
var name _ = name 

mkTmpStVar :: String -> String
mkTmpStVar name = "tmp_" ++ name

mkUpdateStFunc :: String -> String
mkUpdateStFunc name = "update_state_" ++ name

mkQueueVar :: String -> String
mkQueueVar name = "queue_" ++ name 

mkQueuePtrVar :: String -> String
mkQueuePtrVar name = "ptr_" ++ name

