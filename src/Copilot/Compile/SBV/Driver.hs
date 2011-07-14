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
--import Copilot.Compile.SBV.Queue (QueueSize)
--import Copilot.Compile.SBV.Common
import Copilot.Compile.SBV.MetaTable

import qualified Data.SBV as S
import qualified Data.SBV.Internals as S

import qualified Copilot.Core as C

-- Store pairs of variables and the functions that update their values.
-- data VarFunc a = VarFunc 
--   { getVar  :: Var a
--   , getFunc :: Var a }

-- data QueueState = forall a. QueueState
--   { getQueue    :: VarFunc a
--   , getQueuePtr :: VarFunc QueueSize }

-- data TmpState = forall a. TmpState 
--   { getTmp :: VarFunc a }

-- data SampleVar = forall a. SampleVar
--   { getSampleVar :: Var a 
--   , getSbvVar    :: S.SBVCodeGen (S.SBV a) }

-- data Driver = Driver 
--   { getQueues :: [QueueState]
--   , getTmps   :: [TmpState]
--   , getSamps  :: [SampleVar] }

-- type DriverSt a = State Driver a

driver :: MetaTable -> C.Spec ->  IO ()
driver meta spec =
-- MetaTable { streamInfoMap = strMap
--                       , externInfoMap = extMap } =
  putStrLn $ renderStyle (style {lineLength = 80}) code
  where 
  code = 
       preCode spec meta 
    $$ declareVars spec meta 
    $$ updateStates spec meta 
    $$ fireTrigger undefined meta 
    $$ updateBuffers spec meta  
    $$ updatePtrs meta 

preCode :: C.Spec -> MetaTable -> Doc
preCode spec MetaTable { streamInfoMap = strMap
                       , externInfoMap = extMap } = empty

declareVars :: C.Spec -> MetaTable -> Doc
declareVars spec MetaTable { streamInfoMap = strMap
                           , externInfoMap = extMap } = empty

updateStates :: C.Spec -> MetaTable -> Doc
updateStates spec MetaTable { streamInfoMap = strMap
                            , externInfoMap = extMap } = undefined

fireTrigger :: [C.Trigger] -> MetaTable -> Doc
fireTrigger triggers meta = undefined

updateBuffers :: C.Spec -> MetaTable -> Doc
updateBuffers spec MetaTable { streamInfoMap = strMap
                             , externInfoMap = extMap } = undefined

updatePtrs :: MetaTable -> Doc
updatePtrs MetaTable { streamInfoMap = strMap } =
  vcat $ map updateFunc pointers
  where 
  pointers = undefined
--    let strmInM.toList strMap

  updateFunc :: (String, Int) -> Doc
  updateFunc (ptr, sz) =
    text ptr <+> equals <> text "+" <+> int 1 <> semi
    
