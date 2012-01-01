--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, GADTs #-}

-- | Generates the code around the SBV functions to hold the state-updates,
-- external variables, etc.

module Copilot.Compile.SBV.Driver
  ( driver
  , driverName
  ) where

import Prelude hiding (id)
import qualified Data.Map as M
import Data.List (intersperse)
import qualified System.IO as I
import Text.PrettyPrint.HughesPJ

import Copilot.Compile.SBV.MetaTable
import Copilot.Compile.SBV.Queue (QueueSize)
import Copilot.Compile.SBV.Common
import Copilot.Compile.SBV.Params

import qualified Copilot.Core as C
--import Copilot.Core.Type.Equality ((=~=), coerce, cong)
import qualified Copilot.Core.Type.Show as C (showWithType, ShowType(..))
import Copilot.Compile.Header.C99 (c99HeaderName)

--------------------------------------------------------------------------------

driverName :: Params -> String
driverName params = withPrefix (prefix params) "driver" ++ ".c"

--------------------------------------------------------------------------------

-- | Define a C function.
mkFunc :: String -> Doc -> Doc
mkFunc fnName doc =
     text "void" <+> text fnName
       <> lparen <> text "void" <> rparen <+> lbrace
  $$ nest 2 doc $$ nest 0 rbrace

mkArgs :: [Doc] -> Doc
mkArgs args = hsep (punctuate comma args)

-- | Call a C function.
mkFuncCall :: String -> [Doc] -> Doc
mkFuncCall f args = text f <> lparen <> mkArgs args <> rparen

--------------------------------------------------------------------------------

driver :: Params -> MetaTable -> C.Spec -> String -> String -> IO ()
driver params meta (C.Spec streams observers _) dir fileName = do
  let filePath = dir ++ '/' : driverName params
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
  wr (text "#include" <+> doubleQuotes (text $ c99HeaderName (prefix params)))
  wr (text "")

  wr (declObservers (prefix params) observers)
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
    mkFunc (withPrefix (prefix params) "step")
           (   mkFuncCall sampleExtsF    [] <> semi
            $$ mkFuncCall updateStatesF  [] <> semi
            $$ mkFuncCall updateBuffersF [] <> semi
            $$ mkFuncCall updatePtrsF    [] <> semi
            $$ mkFuncCall observersF     [] <> semi
            $$ mkFuncCall triggersF      [] <> semi
           )

  copilot = vcat $ intersperse (text "")
    [ sampleExts meta
    , updateStates streams
    , updateBuffers meta
    , updatePtrs meta 
    , updateObservers params meta
    , fireTriggers meta
    ]

--------------------------------------------------------------------------------

-- Declare global variables.

data Decl = Decl { retT    :: Doc
                 , declVar :: Doc
                 , initVal :: Doc }

varDecls :: MetaTable -> Doc
varDecls meta = vcat $ map varDecl (getVars meta)

  where
  getVars :: MetaTable -> [Decl] 
  getVars MetaTable { streamInfoMap = streams 
                    , externVarInfoMap = externs } = 
       map getTmpStVars (M.toList streams)
    ++ map getQueueVars (M.toList streams)
    ++ map getQueuePtrVars (map fst $ M.toList streams)
    ++ map getExtVars (M.toList externs)

  getTmpStVars :: (C.Id, C.Stream) -> Decl
  getTmpStVars (id, C.Stream { C.streamExprType  = t
                               , C.streamBuffer = que }) = 
    Decl (retType t) (text $ mkTmpStVar id) getFirst
    where 
    -- ASSUME queue is nonempty!
    getFirst = text (cShow $ C.showWithType C.Haskell t (headErr que))
    headErr [] = C.impossible "headErr" "copilot-sbv"
    headErr xs = head xs
  
  getQueueVars :: (C.Id, C.Stream) -> Decl
  getQueueVars (id, C.Stream { C.streamExprType = t
                             , C.streamBuffer = que }) =
    Decl (retType t) 
         (text (mkQueueVar id) <> lbrack <> int (length que) <> rbrack)
         getInits
    where 
    getInits = lbrace <+> vals <+> rbrace
      where 
      vals = hcat $ punctuate (comma <> text " ") 
                              (map (text . cShow . C.showWithType C.Haskell t) 
                                   que)

  getQueuePtrVars :: C.Id -> Decl
  getQueuePtrVars id = 
    Decl (retType queSize) (text $ mkQueuePtrVar id) (int 0)
    where 
    queSize :: C.Type QueueSize
    queSize = C.typeOf 

  getExtVars :: (C.Name, C.ExtVar) -> Decl
  getExtVars (var, C.ExtVar _ (C.UType { C.uTypeType = t })) = 
    Decl (retType t) (text $ mkExtTmpVar var) (int 0)

  varDecl :: Decl -> Doc
  varDecl Decl { retT = t, declVar = v, initVal = i } =
    t <+> v <+> equals <+> i <> semi

  cShow :: String -> String
  cShow "True"  = show (1::Int)
  cShow "False" = show (0::Int)
  cShow x       = x

--------------------------------------------------------------------------------

declObservers :: Maybe String -> [C.Observer] -> Doc
declObservers prfx = vcat . map declObserver

  where
  declObserver :: C.Observer -> Doc
  declObserver
    C.Observer
      { C.observerName     = name
      , C.observerExprType = t } =
    retType t <+> text (withPrefix prfx name) <> semi

--------------------------------------------------------------------------------

sampleExts :: MetaTable -> Doc
sampleExts MetaTable { externVarInfoMap = extVMap
                     , externArrInfoMap = extAMap
                     , externFunInfoMap = extFMap } 
  =
  -- Arrays and functions have to come after vars.  This is because we may use
  -- the assignment of extVars in the definition of extArrs.  We could write it
  -- differently, but it's easier.  The Analyzer.hs copilot-core prevents arrays
  -- or functions from being used in arrays or functions.
  mkFunc sampleExtsF $ vcat (extVars ++ extArrs ++ extFuns)

  where
  extVars = map sampleVExt ((fst . unzip . M.toList) extVMap)
  extArrs = map sampleAExt (M.toList extAMap)
  extFuns = map sampleFExt (M.toList extFMap)

--------------------------------------------------------------------------------

-- Variables
sampleVExt :: C.Name -> Doc
sampleVExt name = 
  text (mkExtTmpVar name) <+> equals <+> text name <> semi

--------------------------------------------------------------------------------
-- Arrays

-- Currenty, Analyze.hs in copilot-language forbids recurssion in
-- external arrays or functions (i.e., an external array can't use another
-- external array to compute it's index), so a lot of what is below isn't
-- currently necessary.
sampleAExt :: (C.Name, C.ExtArray) -> Doc
sampleAExt (name, C.ExtArray { C.externArrayIdx = idx })
  = 
  text (mkExtTmpVar name) <+> equals <+> arrIdx name idx
 
  where 
  arrIdx :: C.Name -> C.Expr a -> Doc
  arrIdx name' e = 
    text name' <> lbrack <> idxFCall e <> rbrack <> semi

  -- Ok, because the analyzer disallows arrays or function calls in index
  -- expressions, and we assign all variables before arrays.
  idxFCall :: C.Expr a -> Doc
  idxFCall e = 
    mkFuncCall (mkExtArrFn name) (map text $ collectArgs e)

--------------------------------------------------------------------------------

-- External functions
sampleFExt :: (C.Name, C.ExtFun) -> Doc
sampleFExt (name, C.ExtFun { C.externFunArgs = args })
  = 
  text (mkExtTmpVar name) <+> equals <+> text name <> lparen
    <+> hsep (punctuate comma $ map mkArgCall (zip [(0 :: Int) ..] args))
    <> rparen <> semi

     where
     mkArgCall = undefined--  (i, ( C.UType { C.uTypeType = t}

--------------------------------------------------------------------------------

updateStates :: [C.Stream] -> Doc
updateStates streams =
  mkFunc updateStatesF $ vcat $ map updateSt streams
  where
  updateSt :: C.Stream -> Doc
  updateSt C.Stream { C.streamId   = id
                    , C.streamExpr = e } =
    text (mkTmpStVar id) <+> equals
      <+> mkFuncCall (mkUpdateStFn id)
                     (map text $ collectArgs e)
      <>  semi

--------------------------------------------------------------------------------

updateObservers :: Params -> MetaTable -> Doc
updateObservers params MetaTable { observerInfoMap = observers } 
  =
  mkFunc observersF $ vcat $ map updateObsv (M.toList observers)
  where
  updateObsv :: (C.Name, ObserverInfo) -> Doc
  updateObsv (name, ObserverInfo { observerArgs = args }) =
    text (withPrefix (prefix params) name) <+> text "=" <+>
    mkFuncCall (mkObserverFn name) (map text args) <> semi

--------------------------------------------------------------------------------

fireTriggers :: MetaTable -> Doc
fireTriggers MetaTable { triggerInfoMap = triggers } 
  =
  mkFunc triggersF $ vcat $ map fireTrig (M.toList triggers)

  where
  -- if (guard) trigger(args);
  fireTrig :: (C.Name, TriggerInfo) -> Doc
  fireTrig (name, TriggerInfo { guardArgs      = gArgs
                              , triggerArgArgs = argArgs }) 
    = 
    let f = mkFuncCall name (map mkArg (mkTriggerArgIdx argArgs)) <> semi in
    text "if" <+> lparen <> guardF <> rparen $+$ nest 2 f

    where
    guardF :: Doc
    guardF = mkFuncCall (mkTriggerGuardFn name) (map text gArgs)

    mkArg :: (Int, [String]) -> Doc
    mkArg (i, args) =
      mkFuncCall (mkTriggerArgFn i name) (map text args)

--------------------------------------------------------------------------------

updateBuffers :: MetaTable -> Doc
updateBuffers MetaTable { streamInfoMap = strMap } 
  =
  mkFunc updateBuffersF $ vcat $ map updateBuf (M.toList strMap)

  where
  updateBuf :: (C.Id, C.Stream) -> Doc
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
  varAndUpdate :: (C.Id, C.Stream) -> Doc
  varAndUpdate (id, C.Stream { C.streamBuffer = que }) =
    updateFunc (fromIntegral $ length que) (mkQueuePtrVar id)

  -- idx = (idx + 1) % queueSize;
  updateFunc :: QueueSize -> String -> Doc
  updateFunc sz ptr =
    text ptr <+> equals 
      <+> lparen <> text ptr <+> text "+" <+> int 1 <> rparen 
      <+> text "%" <+> int (fromIntegral sz) <> semi

--------------------------------------------------------------------------------

sampleExtsF, triggersF, observersF, updatePtrsF :: String
updateBuffersF, updateStatesF :: String
updatePtrsF    = "updatePtrs"
updateBuffersF = "updateBuffers"
updateStatesF  = "updateStates"
triggersF      = "fireTriggers"
observersF     = "updateObservers"
sampleExtsF    = "sampleExts"

--------------------------------------------------------------------------------

retType :: C.Type a -> Doc
retType t = text $
  case t of
    C.Bool  -> "SBool"

    C.Int8  -> "SInt8"
    C.Int16 -> "SInt16"
    C.Int32 -> "SInt32"
    C.Int64 -> "SInt64"

    C.Word8  -> "SWord8"
    C.Word16 -> "SWord16"
    C.Word32 -> "SWord32"
    C.Word64 -> "SWord64"

    _          -> C.badUsage "You've tried to compile a Copilot program to SBV with a type SBV does not support.  SBV does not support floats or doubles.  To compile programs using these types, use the copilot-c99 (Atom) backend.  See README.md for more information."
