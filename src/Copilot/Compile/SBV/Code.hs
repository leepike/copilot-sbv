--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.SBV.Code
  ( schedule
  ) where

import Copilot.Compile.SBV.Copilot2SBV (c2sExpr)
import Copilot.Compile.SBV.MetaTable
  (MetaTable (..), StreamInfo (..), ExternInfo (..))
import qualified Copilot.Compile.SBV.Queue as Q
import qualified Copilot.Compile.SBV.Witness as W

import qualified Copilot.Core as C
import Copilot.Core.Type.Equality ((=~=), coerce, cong)

import qualified Data.SBV as S

import Data.List (intersperse)
import qualified Data.Map as M
import Prelude hiding (id)

--------------------------------------------------------------------------------

-- -- XXXX
-- updateStates :: C.Spec -> S.SBVCodeGen ()
-- updateStates (C.Spec _ [obs] _) = 
--   updateState obs
--   where 
--   updateState :: C.Observer -> S.SBVCodeGen ()
--   updateState (C.Observer id expr t) = do 
--     let e = c2sExpr expr 
--     W.SymWordInst <- return $ W.symWordInst t
--     W.HasSignAndSizeInst <- return $ W.hasSignAndSizeInst t
--     -- XXX FIX
--     Just p <- return (t =~= t)
--     S.cgReturn $ coerce (cong p) e


-- data Phase
--   = SampleExterns
--   | UpdateStates
--   | FireTriggers
--   | UpdateBuffers
--   | UpdateObservers
--   deriving (Bounded, Eq, Enum, Ord, Show)

-- numberOfPhases :: Int
-- numberOfPhases = succ (fromEnum (maxBound :: Phase))

--------------------------------------------------------------------------------

schedule :: MetaTable -> C.Spec -> S.SBVCodeGen ()
schedule meta spec =

-- We just take the current values of externals, so no explicit sampling
--    required.  
--  sampleExterns meta >>
    updateStates    meta spec >>
    fireTriggers    meta spec >>
    updateBuffers   meta spec

--------------------------------------------------------------------------------

-- sampleExterns :: MetaTable -> S.SBVCodeGen ()
-- sampleExterns = undefined
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

updateStates :: MetaTable -> C.Spec -> S.SBVCodeGen ()
updateStates meta (C.Spec streams _ _) = 
  do mapM_ updateStreamState streams
        
  where

  updateStreamState :: C.Stream -> S.SBVCodeGen ()
  updateStreamState (C.Stream id _  _ e t1) =
    do
      e' <- c2sExpr meta e
      let Just strmInfo = M.lookup id (streamInfoMap meta)
      updateStreamState1 t1 id e' strmInfo

  updateStreamState1 :: 
    C.Type a -> C.Id -> S.SBV a -> StreamInfo -> S.SBVCodeGen ()
  updateStreamState1 t1 id e1 (StreamInfo _ tmpVar t2) = do
    W.SymWordInst <- return (W.symWordInst t2)
    W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t2)
    Just p       <- return (t1 =~= t2)
    S.cgReturn $ coerce (cong p) e1

{-
  updateLet :: C.Let -> Atom ()
  updateLet
    C.Let
      { C.letVar  = name
      , C.letExpr = e
      , C.letType = t1
      } =
    let
      Just letInfo = M.lookup name (letInfoMap meta)
    in
      updateLet1 t1 name (c2aExpr meta e) letInfo

  updateLet1 :: C.Type a -> C.Name -> A.E a -> LetInfo -> Atom ()
  updateLet1 t1 name e1
    LetInfo
      { letInfoVar  = v
      , letInfoType = t2
      } =
    exactPhase (fromEnum UpdateStates) $
      atom ("update_let_" ++ name) $
        do
          W.AssignInst <- return (W.assignInst t2)
          Just p <- return (t1 =~= t2)
          v <== coerce (cong p) e1
-}

--------------------------------------------------------------------------------

fireTriggers :: MetaTable -> C.Spec -> S.SBVCodeGen ()
fireTriggers meta (C.Spec _ _ triggers) =
  mapM_ fireTrigger triggers

  where

-- XXX there's no way to call an external function from within SBV-generated
-- code.  So the best we can do is ask if the guard is true, and if so, call a C
-- function ourselves with the arguments.  For now, let's just return the boolean.
  fireTrigger :: C.Trigger -> S.SBVCodeGen ()
  fireTrigger (C.Trigger name guardExp args) = do
--    exactPhase (fromEnum FireTriggers) $
--      atom ("fire_trigger_" ++ name) $
--    args' <- mapM triggerArg2SBV (reverse args)
    exp'  <- c2sExpr meta guardExp
    S.cgReturn exp'
    -- cond exp'
    -- A.action fnCall args'

    where

    -- triggerArg2SBV :: C.TriggerArg -> S.SBVCodeGen (S.SBV a)
    -- triggerArg2SBV (C.TriggerArg e t) = c2sExpr meta e
--      case W.symWordInst t of W.SymWordInst -> 
        

        -- case W.exprInst t of
        --   W.ExprInst -> A.ue (

    -- fnCall :: [String] -> String
    -- fnCall xs = name ++ "(" ++ concat (intersperse "," xs) ++ ")"

--------------------------------------------------------------------------------

updateBuffers :: MetaTable -> C.Spec -> S.SBVCodeGen ()
updateBuffers meta (C.Spec streams _ _) = 
  mapM_ updateBuffer streams

  where

  updateBuffer :: C.Stream -> S.SBVCodeGen ()
  updateBuffer C.Stream { C.streamId = id } =
    let Just strmInfo = M.lookup id (streamInfoMap meta)
    in  updateBuffer1 id strmInfo

  updateBuffer1 :: C.Id -> StreamInfo -> S.SBVCodeGen ()
  updateBuffer1 id (StreamInfo queue tmpVar t) = do
    W.SymWordInst <- return (W.symWordInst t)
    W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
    t <- S.cgInput tmpVar
    Q.dropFirstElemAndSnoc t queue

--------------------------------------------------------------------------------
