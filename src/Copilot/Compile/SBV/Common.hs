--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.SBV.Common
  ( Var
  , var
  ) where

type Var a = String

var :: String -> a -> Var a
var name _ = name 
