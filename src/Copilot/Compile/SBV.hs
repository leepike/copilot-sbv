--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Compile.SBV
  ( compile
  ) where

import qualified Copilot.Core as C
import qualified Data.SBV as S
import Copilot.Compile.SBV.Code (schedule)
import Copilot.Compile.SBV.MetaTable (allocMetaTable)
--import Copilot.Compile.SBV.Copilot2SBV
--------------------------------------------------------------------------------

compile :: String -> C.Spec -> IO ()
compile fileName spec = 
  S.compileToCLib 
    Nothing 
    fileName 
    (schedule (allocMetaTable spec) spec)

--------------------------------------------------------------------------------
