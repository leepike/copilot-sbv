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
import Data.List (intersperse)
import qualified System.IO as I
import Text.PrettyPrint.HughesPJ

import Copilot.Compile.SBV.MetaTable
import Copilot.Compile.SBV.Queue (Queue(..))
import Copilot.Compile.SBV.Common

import qualified Copilot.Core as C

--------------------------------------------------------------------------------

-- | Define a C function.
mkFunc :: String -> Doc -> Doc
mkFunc fnName doc =
     text "void" <+> text fnName 
       <> lparen <> text "void" <> rparen <+> lbrace
  $$ nest 2 doc $$ nest 0 rbrace

mkArgs :: [Doc] -> Doc
mkArgs args = hsep (punctuate comma args)

mkFuncCall :: String -> [Doc] -> Doc
mkFuncCall f args = text f <> lparen <> mkArgs args <> rparen 

--------------------------------------------------------------------------------

driver :: MetaTable -> String -> String -> IO ()
driver meta dir fileName = do
  let filePath = dir ++ '/' : ("copilot_driver_" ++ fileName ++ ".c")
  h <- I.openFile filePath I.WriteMode
  let wr doc = I.hPutStrLn h (mkStyle doc)
  
  wr (    text "/*" 
      <+> text "Driver for SBV program generated from Copilot." 
      <+> text "*/")
  wr (text "/*" <+> text "Edit as you see fit" <+> text "*/")
  wr (text "")
  
  wr (text "#include <inttypes.h>")
  wr (text "#include <stdbool.h>")
  wr (text "#include <stdint.h>")
  wr (text "#include <stdio.h>")
  wr (text "#include" <+> doubleQuotes (text fileName <> text ".h"))
  wr (text "")

  wr (varDecls meta)
  wr (text "")

  wr copilot
  wr (text "")
  wr driverFn

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

  copilot = vcat $ intersperse (text "")
    [ sampleExts meta 
    , updateStates meta
    , fireTriggers meta
    , updateBuffers meta  
    , updatePtrs meta ]

--------------------------------------------------------------------------------

data Decl = Decl { retT    :: Doc
                 , declVar :: Doc }

varDecls :: MetaTable -> Doc
varDecls meta = vcat $ map varDecl (getVars meta)
  where

  getVars :: MetaTable -> [Decl] 
  getVars MetaTable { streamInfoMap = streams 
                    , externInfoMap = externs } = 
       map getTmpStVars (M.toList streams)
    ++ map getQueueVars (M.toList streams)
    ++ map getQueuePtrVars (M.toList streams)
    ++ map getExtVars (M.toList externs)

  getTmpStVars :: (C.Id, StreamInfo) -> Decl
  getTmpStVars (id, StreamInfo { streamInfoType = t }) = 
    Decl (retType t) (text $ mkTmpStVar id)
  
  getQueueVars :: (C.Id, StreamInfo) -> Decl
  getQueueVars (id, StreamInfo { streamInfoType = t }) = 
    Decl (retType t) (text $ "*" ++ mkQueueVar id)

  getQueuePtrVars :: (C.Id, StreamInfo) -> Decl
  getQueuePtrVars (id, StreamInfo { streamInfoType = t }) = 
    Decl (retType t) (text $ mkQueuePtrVar id)

  getExtVars :: (C.Name, ExternInfo) -> Decl
  getExtVars (var, ExternInfo { externInfoType = t }) = 
    Decl (retType t) (text $ mkExtTmpVar var)

  varDecl :: Decl -> Doc
  varDecl Decl { retT = t, declVar = v } =
    t <+> v <> semi

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
    text "if" <+> lparen <> guardF <> rparen <+> 
      mkFuncCall name (map mkArg (mkTriggerArgIdx argArgs)) <> semi
    where
    guardF :: Doc
    guardF = mkFuncCall (mkTriggerGuardFn name) (map text gArgs)
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
  varAndUpdate (id, StreamInfo { streamInfoQueue = que }) =
    updateFunc (size que) (mkQueuePtrVar id)

  -- idx = (idx + 1) % queueSize;
  updateFunc :: Int -> String -> Doc
  updateFunc sz ptr =
    text ptr <+> equals 
      <+> lparen <+> text ptr <+> text "+" <+> int 1 <> rparen 
      <+> text "%" <+> int sz <> semi

--------------------------------------------------------------------------------

sampleExtsF, triggersF, updatePtrsF, updateBuffersF, updateStatesF :: String
updatePtrsF    = "updatePtrs"
updateBuffersF = "updateBuffers"
updateStatesF  = "updateStates"
triggersF      = "fireTriggers"
sampleExtsF    = "sampleExts"

--------------------------------------------------------------------------------

retType :: C.Type a -> Doc
retType t = text $
  case t of
    C.Bool  _ -> "SBool"

    C.Int8  _ -> "SInt8"
    C.Int16 _ -> "SInt16"
    C.Int32 _ -> "SInt32"
    C.Int64 _ -> "SInt64"

    C.Word8  _ -> "SWord8"
    C.Word16 _ -> "SWord16"
    C.Word32 _ -> "SWord32"
    C.Word64 _ -> "SWord64"

    _          -> error "Error in retType: non-SBV type."

