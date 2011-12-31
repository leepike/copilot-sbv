--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.SBV
  ( compile
  , sbvDirName
  , module Copilot.Compile.SBV.Params
  ) where

import qualified Copilot.Core as C
import Copilot.Compile.Header.C99 (c99HeaderName, genC99Header)

import qualified Data.SBV as S

import Copilot.Compile.SBV.Driver (driver, driverName)
import Copilot.Compile.SBV.Makefile (makefile, makefileName)
import Copilot.Compile.SBV.Code 
  (updateStates, updateObservers, fireTriggers, getExtArrs)
import Copilot.Compile.SBV.MetaTable (allocMetaTable)
import Copilot.Compile.SBV.Params

--------------------------------------------------------------------------------

-- Note: we put everything in a directory named by the dirName.

sbvDirName :: String
sbvDirName = "copilot-sbv"

compile :: Params -> C.Spec -> IO ()
compile params spec = do
  let meta    = allocMetaTable spec
      sbvName = withPrefix (prefix params) "internal"
  putStrLn "Compiling SBV-generated functions .."

  S.compileToCLib
    (Just sbvDirName)
    sbvName
    (  updateStates    meta spec
    ++ updateObservers meta spec
    ++ fireTriggers    meta spec 
    ++ getExtArrs      meta 
    )

  putStrLn ""
  putStrLn $ "Generating Copilot driver " ++ driverName params ++ " .."
  driver params meta spec sbvDirName sbvName

  putStrLn ""
  putStrLn $ "Generating Copilot header " ++ c99HeaderName (prefix params) ++ " .."
  genC99Header (prefix params) sbvDirName spec

  putStrLn ""
  putStrLn $ "Generating Copilot driver Makefile rules .."
               ++ makefileName params ++ " .."
  makefile params sbvDirName sbvName

  putStrLn "Done."

--------------------------------------------------------------------------------
