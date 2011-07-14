--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Compile.SBV
  ( compile
  ) where

import qualified Copilot.Core as C
import qualified Data.SBV as S

import Copilot.Compile.SBV.Driver (driver)
import Copilot.Compile.SBV.Code (updateStates)
import Copilot.Compile.SBV.MetaTable (allocMetaTable)
--import Copilot.Compile.SBV.Copilot2SBV
--------------------------------------------------------------------------------

compile :: String -> C.Spec -> IO ()
compile fileName spec = do 
  let meta = allocMetaTable spec

  putStrLn "Compiling ..."

  S.compileToCLib 
    Nothing 
    fileName
    (updateStates meta spec)
    
  putStrLn "Creating Driver ..."

  driver meta spec

  putStrLn "Done ..."

--------------------------------------------------------------------------------
