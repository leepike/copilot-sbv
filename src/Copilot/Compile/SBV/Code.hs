--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.SBV.Code
  ( updateStates
  ) where

import Copilot.Compile.SBV.Copilot2SBV (c2sExpr)
import Copilot.Compile.SBV.MetaTable
  (MetaTable (..), StreamInfo (..)) -- XXX , ExternInfo (..))
import qualified Copilot.Compile.SBV.Queue as Q
import qualified Copilot.Compile.SBV.Witness as W
import Copilot.Compile.SBV.Common

import qualified Copilot.Core as C
import Copilot.Core.Type.Equality ((=~=), coerce, cong)

import qualified Data.SBV as S
import qualified Data.SBV.Internals as S

import qualified Data.Map as M
import Prelude hiding (id)

--------------------------------------------------------------------------------

type SBVFunc  = (String, S.SBVCodeGen ())

--type SBVFuncs = [SBVFunc]

--------------------------------------------------------------------------------

-- schedule :: MetaTable -> C.Spec -> SBVFuncs
-- schedule meta spec =

-- We just take the current values of externals, so no explicit sampling
--    required.  
--  sampleExterns meta >>
--    updateStates    meta spec 
--    fireTriggers    meta spec 
--    updatePointers  meta spec

--------------------------------------------------------------------------------

-- sampleExterns :: MetaTable -> S.SBVCodeGen ()
-- sampleExterns = 
  -- mapM_ sampleExtern . M.toList . externInfoMap

  -- where

  -- sampleExtern :: (C.Name, ExternInfo) -> Atom ()
  -- sampleExtern (name, ExternInfo v t) =
  --   exactPhase (fromEnum SampleExterns) $
  --     atom ("sample_" ++ name) $
  --       do
  --         W.AssignInst <- return $ W.assignInst t
  --         v <== A.value (A.var' name (c2aType t))

--------------------------------------------------------------------------------

updateStates :: MetaTable -> C.Spec -> [SBVFunc]
updateStates meta (C.Spec streams _ _) = 
  do map updateStreamState streams
        
  where
  mkSBVFunc :: String -> S.SBVCodeGen () -> (String, S.SBVCodeGen ())
  mkSBVFunc str codeGen = (str, codeGen)

  updateStreamState :: C.Stream -> SBVFunc
  updateStreamState C.Stream { C.streamId       = id
                             , C.streamExpr     = e
                             , C.streamExprType = t1
                                                      } =
    mkSBVFunc (mkUpdateStFunc id) $
    do e' <- c2sExpr meta e
       let Just strmInfo = M.lookup id (streamInfoMap meta)
       updateStreamState1 t1 e' strmInfo

  updateStreamState1 :: C.Type a -> S.SBV a -> StreamInfo -> S.SBVCodeGen ()
  updateStreamState1 t1 e1 (StreamInfo _ _ t2) = do
    W.SymWordInst <- return (W.symWordInst t2)
    W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t2)
    Just p <- return (t1 =~= t2)
    S.cgReturn $ coerce (cong p) e1

--------------------------------------------------------------------------------

-- fireTriggers :: MetaTable -> C.Spec -> SBVFuncs
-- fireTriggers meta (C.Spec _ _ triggers) =
--   map fireTrigger triggers

--   where

-- -- XXX there's no way to call an external function from within SBV-generated
-- -- code.  So the best we can do is ask if the guard is true, and if so, call a C
-- -- function ourselves with the arguments.  For now, let's just return the boolean.
--   fireTrigger :: C.Trigger -> SBVFunc
--   fireTrigger (C.Trigger name guardExp _) = 
--     mkSBVFunc ("trigger_" ++ name ++ "_guard") $ 
-- --    exactPhase (fromEnum FireTriggers) $
-- --      atom ("fire_trigger_" ++ name) $
-- --    args' <- mapM triggerArg2SBV (reverse args)
--     do 
--       exp'  <- c2sExpr meta guardExp
--       S.cgReturn exp'
--     -- cond exp'
    -- A.action fnCall args'

--    where

    -- triggerArg2SBV :: C.TriggerArg -> S.SBVCodeGen (S.SBV a)
    -- triggerArg2SBV (C.TriggerArg e t) = c2sExpr meta e
--      case W.symWordInst t of W.SymWordInst -> 
        

        -- case W.exprInst t of
        --   W.ExprInst -> A.ue (

    -- fnCall :: [String] -> String
    -- fnCall xs = name ++ "(" ++ concat (intersperse "," xs) ++ ")"

--------------------------------------------------------------------------------

-- updatePointers :: MetaTable -> C.Spec -> SBVFuncs
-- updatePointers meta C.Spec { C.specStreams = streams } =
--   map updateBuffer streams

--   where

  -- updateBuffer :: C.Stream -> SBVFunc
  -- updateBuffer C.Stream { C.streamId = id } =
  --   let Just strmInfo = M.lookup id (streamInfoMap meta) in
  --   mkSBVFunc ("update_pointer_" ++ show id) (updatePointer1 strmInfo)

  -- updatePointer1 :: StreamInfo -> S.SBVCodeGen ()
  -- updatePointer1 (StreamInfo queue tmpVar t) = do
  --   -- W.SymWordInst <- return (W.symWordInst t)
  --   -- W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
  --  input <- S.cgInput tmpVar

  --  Q.dropFirstElemAndSnoc queue

--------------------------------------------------------------------------------
