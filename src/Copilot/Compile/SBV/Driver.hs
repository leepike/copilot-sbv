--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

-- | Generates the code around the SBV functions to hold the state-updates,
-- external variables, etc.

module Copilot.Compile.SBV.Driver
  ( driver 
  ) where

--import Control.Monad.State

import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ
import Copilot.Compile.SBV.Queue
--import Copilot.Compile.SBV.Common
import Copilot.Compile.SBV.MetaTable
import Copilot.Compile.SBV.Common

import qualified Data.SBV as S
import qualified Data.SBV.Internals as S

import qualified Copilot.Core as C

--------------------------------------------------------------------------------

driver :: MetaTable -> C.Spec ->  IO ()
driver meta spec =
  putStrLn $ renderStyle (style {lineLength = 80}) code
  where 
  code = 
       preCode meta 
    $$ declareVars meta 
    $$ sampleExts meta 
    $$ updateStates meta spec
    $$ fireTrigger undefined meta 
    $$ updateBuffers meta  
    $$ updatePtrs meta 
    $$ mkMain  

--------------------------------------------------------------------------------

preCode :: MetaTable -> Doc
preCode MetaTable { streamInfoMap = strMap
                  , externInfoMap = extMap } = undefined

--------------------------------------------------------------------------------

declareVars :: MetaTable -> Doc
declareVars MetaTable { streamInfoMap = strMap
                      , externInfoMap = extMap } = undefined

--------------------------------------------------------------------------------

sampleExts :: MetaTable -> Doc
sampleExts MetaTable { streamInfoMap = strMap
                     , externInfoMap = extMap } = undefined 
--  mkFunc "sampleExts" $ vcat 
--------------------------------------------------------------------------------

updateStates :: MetaTable -> C.Spec -> Doc
updateStates MetaTable { streamInfoMap = strMap } spec = undefined
  -- mkFunc updateStatesF $ vcat $ map updateSt (M.toList strMap)

  -- where 
  -- -- tmp_X = updateState(arg0, arg1, ... );
  -- updateSt :: (C.Id, StreamInfo) -> Doc
  -- updateSt = (id, StreamInfo { XXX = YYY } = undefined
  --   text mkTmpStVar <+> equals <+> 
  --     lparen <> args (map text XXX) <> 
  --     rparen <> semi

  -- mkArgs :: [Doc] -> Doc
  -- mkArgs = punctuate (comma <+>)

--------------------------------------------------------------------------------

fireTrigger :: [C.Trigger] -> MetaTable -> Doc
fireTrigger triggers meta = undefined

--------------------------------------------------------------------------------

updateBuffers :: MetaTable -> Doc
updateBuffers MetaTable { streamInfoMap = strMap } =
  mkFunc updateBuffersF $ vcat $ map updateBuf (M.toList strMap)

  where
  updateBuf :: (C.Id, StreamInfo) -> Doc
  updateBuf (id, StreamInfo { streamInfoQueue = queue }) =
    updateFunc (mkQueueVar idStr) (mkQueuePtrVar idStr) (mkTmpStVar idStr)
    where idStr :: String
          idStr = show id

  -- queue_strX[ptr] = newVal;
  updateFunc :: String -> String -> String -> Doc
  updateFunc queue ptr tmp =
    text queue <> lbrack <> text ptr <> rbrack <+> equals <+> text tmp <> semi

--------------------------------------------------------------------------------

updatePtrs :: MetaTable -> Doc
updatePtrs MetaTable { streamInfoMap = strMap } =
  mkFunc updatePtrsF $ vcat $ map varAndUpdate (M.toList strMap)

  where 
  varAndUpdate :: (C.Id, StreamInfo) -> Doc
  varAndUpdate (id, StreamInfo { streamInfoQueue = queue }) =
    updateFunc ( mkQueuePtrVar (show id)
               , getSize queue)

  getSize :: Queue a -> Int
  getSize Queue { size = sz } = sz

  -- idx += 1;
  updateFunc :: (String, Int) -> Doc
  updateFunc (ptr, sz) =
    text ptr <+> equals <> text "+" <+> int 1 <> semi

--------------------------------------------------------------------------------

-- | The main.
mkMain :: Doc
mkMain = undefined    

--------------------------------------------------------------------------------

-- | Define a C function.
mkFunc :: String -> Doc -> Doc
mkFunc fnName doc =
  text fnName <> lparen <> text "void" <> rparen <+> lbrace 
    $$ nest 2 doc $$ nest 0 rbrace

--------------------------------------------------------------------------------

updatePtrsF, updateBuffersF, updateStatesF :: String
updatePtrsF    = "updatePtrs"
updateBuffersF = "updateBuffers"
updateStatesF  = "updateStates"
