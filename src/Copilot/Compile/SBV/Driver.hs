--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

-- | Generates the code around the SBV functions to hold the state-updates,
-- external variables, etc.

module Copilot.Compile.SBV.Driver
  ( driver 
  ) where

import Prelude hiding (id)
import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ

import Copilot.Compile.SBV.MetaTable
import Copilot.Compile.SBV.Common

import qualified Copilot.Core as C

--------------------------------------------------------------------------------

-- | Define a C function.
mkFunc :: String -> Doc -> Doc
mkFunc fnName doc =
  text fnName <> lparen <> text "void" <> rparen <+> lbrace 
    $$ nest 2 doc $$ nest 0 rbrace

mkArgs :: [Doc] -> Doc
mkArgs args = hsep (punctuate comma args)

mkFuncCall :: String -> [Doc] -> Doc
mkFuncCall f args = text f <> lparen <> mkArgs args <> rparen 

--------------------------------------------------------------------------------

driver :: MetaTable -> String -> IO ()
driver meta fileName = do
  putStrLn (mkStyle copilot)
  putStrLn ""
  putStrLn (mkStyle driverFn) 

  where 
  mkStyle :: Doc -> String
  mkStyle = renderStyle (style {lineLength = 80})

  driverFn :: Doc 
  driverFn = 
    mkFunc ("driver_" ++ fileName) 
           (   mkFuncCall sampleExtsF    [] <> semi
            $$ mkFuncCall updateStatesF  [] <> semi
            $$ mkFuncCall triggersF      [] <> semi
            $$ mkFuncCall updateBuffersF [] <> semi
            $$ mkFuncCall updatePtrsF    [] <> semi) 

  copilot = 
       preCode meta 
    $$ declareVars meta 
    $$ sampleExts meta 
    $$ updateStates meta
    $$ fireTriggers meta
    $$ updateBuffers meta  
    $$ updatePtrs meta 

--------------------------------------------------------------------------------

preCode :: MetaTable -> Doc
preCode MetaTable { streamInfoMap = strMap
                  , externInfoMap = extMap } = empty

--------------------------------------------------------------------------------

declareVars :: MetaTable -> Doc
declareVars MetaTable { streamInfoMap = strMap
                      , externInfoMap = extMap } = empty

--------------------------------------------------------------------------------

sampleExts :: MetaTable -> Doc
sampleExts MetaTable { externInfoMap = extMap } = 
  mkFunc sampleExtsF $ vcat $ map sampleExt ((fst . unzip . M.toList) extMap) 

  where
  sampleExt :: C.Name -> Doc
  sampleExt name = text (mkExtTmpVar name) <+> equals <+> text name <> semi

--------------------------------------------------------------------------------

updateStates :: MetaTable -> Doc
updateStates MetaTable { streamInfoMap = strMap } = 
  mkFunc updateStatesF $ vcat $ map updateSt (M.toList strMap)

  where 
  -- tmp_X = updateState(arg0, arg1, ... );
  updateSt :: (C.Id, StreamInfo) -> Doc
  updateSt (id, StreamInfo { streamFnInputs = args }) =
    text (mkTmpStVar id) <+> equals 
      <+> mkFuncCall (mkUpdateStFn id) (map text args) <> semi

--------------------------------------------------------------------------------

fireTriggers :: MetaTable -> Doc
fireTriggers MetaTable { triggerInfoMap = triggers } =

  mkFunc triggersF $ vcat $ map fireTrig (M.toList triggers)

  where
  -- if (guard) trigger(args);
  fireTrig :: (C.Name, TriggerInfo) -> Doc
  fireTrig (name, TriggerInfo { guardArgs      = gArgs
                              , triggerArgArgs = argArgs }) = 
    text "if" <+> lparen <> mkFuncCall name (map text gArgs) <> rparen <+> 
      mkFuncCall name (map mkArg (mkTriggerArgIdx argArgs)) <> semi
    where
    mkArg :: (Int, [String]) -> Doc
    mkArg (i, args) = mkFuncCall (mkTriggerArgFn i name) (map text args)

--------------------------------------------------------------------------------

updateBuffers :: MetaTable -> Doc
updateBuffers MetaTable { streamInfoMap = strMap } =
  mkFunc updateBuffersF $ vcat $ map updateBuf (M.toList strMap)

  where
  updateBuf :: (C.Id, StreamInfo) -> Doc
  updateBuf (id, _) =
    updateFunc (mkQueueVar id) (mkQueuePtrVar id) (mkTmpStVar id)

  -- queue_strX[ptr] = newVal;
  updateFunc :: String -> String -> String -> Doc
  updateFunc que ptr tmp =
    text que <> lbrack <> text ptr <> rbrack <+> equals <+> text tmp <> semi

--------------------------------------------------------------------------------

updatePtrs :: MetaTable -> Doc
updatePtrs MetaTable { streamInfoMap = strMap } =
  mkFunc updatePtrsF $ vcat $ map varAndUpdate (M.toList strMap)

  where 
  varAndUpdate :: (C.Id, StreamInfo) -> Doc
  varAndUpdate (id, _) =
    updateFunc (mkQueuePtrVar id)

  -- idx += 1;
  updateFunc :: String -> Doc
  updateFunc ptr =
    text ptr <+> text "+" <> equals <+> int 1 <> semi

--------------------------------------------------------------------------------

sampleExtsF, triggersF, updatePtrsF, updateBuffersF, updateStatesF :: String
updatePtrsF    = "updatePtrs"
updateBuffersF = "updateBuffers"
updateStatesF  = "updateStates"
triggersF      = "fireTriggers"
sampleExtsF    = "sampleExts"
