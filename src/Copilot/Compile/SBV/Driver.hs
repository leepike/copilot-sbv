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
import Copilot.Compile.SBV.Copilot2SBV

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
    $$ fireTriggers undefined meta 
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
sampleExts MetaTable { externInfoMap = extMap } = 
  mkFunc sampleExtsF $ vcat $ map sampleExt ((fst . unzip . M.toList) extMap) 

  where
  -- extVar = var;
  sampleExt :: C.Name -> Doc
  sampleExt name = text (mkExtTmpVar name) <+> equals <+> text name <> semi

--------------------------------------------------------------------------------

updateStates :: MetaTable -> C.Spec -> Doc
updateStates MetaTable { streamInfoMap = strMap } spec = 
  mkFunc updateStatesF $ vcat $ map updateSt (M.toList strMap)

  where 
  -- tmp_X = updateState(arg0, arg1, ... );
  updateSt :: (C.Id, StreamInfo) -> Doc
  updateSt (id, StreamInfo { streamFnInputs = args }) =
    text (mkTmpStVar id) <+> equals <+> 
      lparen <> mkArgs (map text args) <> 
      rparen <> semi

  mkArgs :: [Doc] -> Doc
  mkArgs args = hsep (punctuate comma args)

--------------------------------------------------------------------------------

fireTriggers :: [C.Trigger] -> MetaTable -> Doc
fireTriggers triggers meta = 
  mkFunc triggersF $ vcat $ map fireTrig triggers

  where
  -- if (guard) trigger(args);
  fireTrig :: C.Trigger -> Doc
  fireTrig C.Trigger { C.triggerName  = name
                     , C.triggerGuard = guard
                     , C.triggerArgs  = args } =
    text "if" <+> lparen <> mkGuard guard <> rparen <+> 
      text name <> lparen <> sep (map mkArg args) <> rparen <> semi

  mkGuard :: C2SExpr Bool -> Doc
  mkGuard guard = --do
--    e <- c2sExpr meta guard
    undefined

  mkArg :: C.TriggerArg -> Doc
  mkArg C.TriggerArg { C.triggerArgExpr = e
                     , C.triggerArgType = t } = undefined
 
  sep :: [Doc] -> Doc
  sep  args = hsep (punctuate comma args)

--------------------------------------------------------------------------------

updateBuffers :: MetaTable -> Doc
updateBuffers MetaTable { streamInfoMap = strMap } =
  mkFunc updateBuffersF $ vcat $ map updateBuf (M.toList strMap)

  where
  updateBuf :: (C.Id, StreamInfo) -> Doc
  updateBuf (id, StreamInfo { streamInfoQueue = queue }) =
    updateFunc (mkQueueVar id) (mkQueuePtrVar id) (mkTmpStVar id)

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
    updateFunc ( mkQueuePtrVar id
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
mkMain = mkFunc "main" $ vcat $ undefined

--------------------------------------------------------------------------------

-- | Define a C function.
mkFunc :: String -> Doc -> Doc
mkFunc fnName doc =
  text fnName <> lparen <> text "void" <> rparen <+> lbrace 
    $$ nest 2 doc $$ nest 0 rbrace

--------------------------------------------------------------------------------

sampleExtsF, triggersF, updatePtrsF, updateBuffersF, updateStatesF :: String
updatePtrsF    = "updatePtrs"
updateBuffersF = "updateBuffers"
updateStatesF  = "updateStates"
triggersF      = "fireTriggers"
sampleExtsF    = "sampleExts"
