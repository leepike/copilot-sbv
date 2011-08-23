--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.SBV.Makefile
  ( makefile
  , makefileName 
  ) where

import Copilot.Compile.SBV.Driver (driverName)
import Copilot.Compile.SBV.Params
import Text.PrettyPrint.HughesPJ
import qualified System.IO as I

--------------------------------------------------------------------------------

makefileName :: Params -> String
makefileName params = withPrefix (prefix params) "copilot" ++ ".mk"

--------------------------------------------------------------------------------

makefile :: Params -> String -> String -> IO ()
makefile params dir sbvName = do
  let filePath = dir ++ '/' : (makefileName params)
      fileName = "copilot"
  h <- I.openFile filePath I.WriteMode
  let wr doc = I.hPutStrLn h (mkStyle doc)
  wr (text "# Makefile rules for the Copilot driver.")
  wr (text "")
  wr $ text "driver" <> colon 
        <+> text (driverName params) <+> text fileName <> text ".h"
  wr $ text "\t" 
         <> (hsep [ text "$" <> braces (text "CC")
                  , text "$" <> braces (text "CCFLAGS")
                  , text "$<"
                  , text "-o"
                  , text "$@"
                  , text sbvName <> text ".a"])

  where 
  mkStyle :: Doc -> String
  mkStyle = renderStyle (style {lineLength = 80})

--------------------------------------------------------------------------------

