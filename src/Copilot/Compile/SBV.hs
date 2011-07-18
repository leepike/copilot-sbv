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
import Copilot.Compile.SBV.Code (updateStates, fireTriggers)
import Copilot.Compile.SBV.MetaTable (allocMetaTable)

--------------------------------------------------------------------------------

-- Note: we put everything in a directory named by the fileName.  

compile :: String -> C.Spec -> IO ()
compile fileName spec = do 
  let meta = allocMetaTable spec
  putStrLn "Compiling SBV-generated functions..."

  S.compileToCLib 
    (Just fileName)
    fileName
    (updateStates meta spec ++ fireTriggers meta spec)

  putStrLn ""
  putStrLn $ "Generating: Copilot driver " ++ fileName ++ " .."
  driver meta fileName fileName
  putStrLn "Done."

--------------------------------------------------------------------------------
