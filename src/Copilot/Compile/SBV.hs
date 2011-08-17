--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.SBV
  ( compile
  ) where

import qualified Copilot.Core as C
import Copilot.Compile.Header.C99 (c99HeaderName, genC99Header)

import qualified Data.SBV as S

import Copilot.Compile.SBV.Driver (driver, driverName)
import Copilot.Compile.SBV.Makefile (makefile)
import Copilot.Compile.SBV.Code (updateStates, fireTriggers)
import Copilot.Compile.SBV.MetaTable (allocMetaTable)

--------------------------------------------------------------------------------

-- Note: we put everything in a directory named by the fileName.  

compile :: String -> C.Spec -> IO ()
compile fileName spec = do 
  let meta = allocMetaTable spec
  putStrLn "Compiling SBV-generated functions .."

  S.compileToCLib
    (Just fileName)
    fileName
    (updateStates meta spec ++ fireTriggers meta spec)

  putStrLn ""
  putStrLn $ "Generating Copilot driver " ++ driverName fileName ++ ".c" ++ " .."
  driver meta spec fileName fileName

  putStrLn ""
  putStrLn $ "Generating Copilot header " ++ c99HeaderName fileName ++ " .."
  genC99Header fileName fileName spec

  putStrLn ""
  putStrLn $ "Generating Copilot driver Makefile rules " 
               ++ fileName ++ "_copilot.mk" ++ " .."
  makefile fileName fileName

  putStrLn "Done."

--------------------------------------------------------------------------------
