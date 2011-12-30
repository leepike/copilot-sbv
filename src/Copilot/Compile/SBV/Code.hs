--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}

module Copilot.Compile.SBV.Code
  ( updateStates
  , updateObservers
  , fireTriggers
  , getExtArrs
  ) where

import Copilot.Compile.SBV.Copilot2SBV
import Copilot.Compile.SBV.MetaTable
import qualified Copilot.Compile.SBV.Witness as W
import Copilot.Compile.SBV.Common

import qualified Copilot.Core as C
import Copilot.Core.Type.Equality ((=~=), coerce, cong)

import qualified Data.SBV as S
--import qualified Data.SBV.Internals as S

import qualified Data.Map as M
import Control.Monad (foldM)
import Prelude hiding (id)

--------------------------------------------------------------------------------

type SBVFunc  = (String, S.SBVCodeGen ())

mkSBVFunc :: String -> S.SBVCodeGen () -> (String, S.SBVCodeGen ())
mkSBVFunc str codeGen = (str, codeGen)

--------------------------------------------------------------------------------

updateStates :: MetaTable -> C.Spec -> [SBVFunc]
updateStates meta (C.Spec streams _ _) =
  map updateStreamState streams

  where
  updateStreamState :: C.Stream -> SBVFunc
  updateStreamState C.Stream { C.streamId       = id
                             , C.streamExpr     = e
                             , C.streamExprType = t1
                                                      } 
    = mkSBVFunc (mkUpdateStFn id) $ do
        inputs <- mkInputs meta (c2Args e)
        let e' = c2sExpr inputs e
        let Just strmInfo = M.lookup id (streamInfoMap meta) 
        updateStreamState1 t1 e' strmInfo

  updateStreamState1 :: C.Type a -> S.SBV a -> C.Stream -> S.SBVCodeGen ()
  updateStreamState1 t1 e1 C.Stream { C.streamExprType = t2 }
    = do
    W.SymWordInst <- return (W.symWordInst t2)
    W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t2)
    Just p <- return (t1 =~= t2)
    S.cgReturn $ coerce (cong p) e1

--------------------------------------------------------------------------------

updateObservers :: MetaTable -> C.Spec -> [SBVFunc]
updateObservers meta (C.Spec _ observers _) =
  map updateObs observers

  where
  updateObs :: C.Observer -> SBVFunc
  updateObs C.Observer { C.observerName     = name
                       , C.observerExpr     = e
                       , C.observerExprType = t } =
    mkSBVFunc (mkObserverFn name) mkSBVExp

    where
    mkSBVExp =
      do
        inputs <- mkInputs meta (c2Args e)
        let e' = c2sExpr inputs e
        W.SymWordInst <- return (W.symWordInst t)
        W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
        S.cgReturn e'

--------------------------------------------------------------------------------

fireTriggers :: MetaTable -> C.Spec -> [SBVFunc]
fireTriggers meta (C.Spec _ _ triggers) =
  concatMap fireTrig triggers

  where
  fireTrig :: C.Trigger -> [SBVFunc]
  fireTrig C.Trigger { C.triggerName  = name
                     , C.triggerGuard = guard
                     , C.triggerArgs  = args } =
      mkSBVFunc (mkTriggerGuardFn name) mkSBVExp
    : map (mkTriggerArg name) (mkTriggerArgIdx args)
    where
    mkSBVExp = do
      inputs <- mkInputs meta (c2Args guard)
      let e = c2sExpr inputs guard
      S.cgReturn e

  mkTriggerArg :: String -> (Int, C.UExpr) -> SBVFunc
  mkTriggerArg name (i, C.UExpr { C.uExprExpr = e
                                , C.uExprType = t } ) =
    mkSBVFunc (mkTriggerArgFn i name) mkExpr
    where
    mkExpr = do
      inputs <- mkInputs meta (c2Args e)
      let e' = c2sExpr inputs e
      W.SymWordInst <- return (W.symWordInst t)
      W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
      S.cgReturn e'

--------------------------------------------------------------------------------

-- Generate an SBV function that calculates the Copilot expression to get the
-- next index to sample an external array.
getExtArrs :: MetaTable -> [SBVFunc]
getExtArrs meta@(MetaTable { externArrInfoMap = arrs })
  = map mkIdx (M.toList arrs)
  
  where
  mkIdx :: (C.Name, C.ExtArray) -> SBVFunc
  mkIdx (name, C.ExtArray { C.externArrayIdx     = idx
                          , C.externArrayIdxType = t   })
    = 
    mkSBVFunc (mkExtArrFn name) mkSBVExpr
    where
    mkSBVExpr :: S.SBVCodeGen ()
    mkSBVExpr = do
      inputs <- mkInputs meta (c2Args idx)
      W.SymWordInst <- return (W.symWordInst t)
      W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
      S.cgReturn (c2sExpr inputs idx)

--------------------------------------------------------------------------------

-- mkInputs takes the datatype containing the entire spec (meta) as well as all
-- possible arguments to the SBV function generating the expression.  From those
-- arguments, it then generates in the SBVCodeGen monad---the actual Inputs---
-- the queues to hold streams as well as external variables.

-- XXX MUST be put in the monad in the same order as the function call
-- (argToCall from MetaTable.hs).

mkInputs :: MetaTable -> [Arg] -> S.SBVCodeGen Inputs
mkInputs meta args = 
  foldM argToInput (Inputs [] [] [] []) args 

  where
  argToInput :: Inputs -> Arg -> S.SBVCodeGen Inputs

-----------------------------------------
 
  -- External variables
  argToInput acc (Extern name) = 
    let extInfos = externVarInfoMap meta in
    let Just extInfo = M.lookup name extInfos in
    mkExtInput extInfo

    where 
    mkExtInput :: C.ExtVar -> S.SBVCodeGen Inputs
    mkExtInput (C.ExtVar _ C.UType { C.uTypeType = t }) = do
      ext <- mkExtInput_ t (mkExtTmpVar name)
      return acc { extVars = (name, (ExtInput { extInput = ext 
                                              , extType  = t })
                             ) : extVars acc }

-----------------------------------------

-- External arrays
  argToInput acc (ExternArr name) = 
    let extInfos = externArrInfoMap meta in
    let Just extInfo = M.lookup name extInfos in
    mkExtInput extInfo

    where 
    mkExtInput :: C.ExtArray -> S.SBVCodeGen Inputs
    mkExtInput C.ExtArray { C.externArrayElemType = t }
      = do
      v <- mkExtInput_ t (mkExtTmpVar name)
      return acc { extArrs = (name, ExtInput 
                                      { extInput  = v
                                      , extType   = t }
                             ) : extArrs acc }

-----------------------------------------

-- External functions
  argToInput acc (ExternFun name tag) =
    let extInfos = externFunInfoMap meta in
    let Just extInfo = M.lookup name extInfos in
    mkExtInput extInfo

    where
    mkExtInput :: C.ExtFun -> S.SBVCodeGen Inputs
    mkExtInput C.ExtFun { C.externFunType = t }
      = do
      v <- mkExtInput_ t (mkExtTmpFun name tag)
      return acc { extFuns = (name, ExtInput 
                                      { extInput = v
                                      , extType  = t }
                             ) : extFuns acc }

-----------------------------------------

-- Stream queues
  argToInput acc (Queue id) =
    let strmInfos = streamInfoMap meta in
    let Just strmInfo = M.lookup id strmInfos in
    mkQueInput strmInfo

    where
    mkQueInput :: C.Stream -> S.SBVCodeGen Inputs
    mkQueInput C.Stream { C.streamBuffer = que
                        , C.streamExprType  = t } = do
      arr <- mkQueInput_ t que
      ptr <- S.cgInput (mkQueuePtrVar id)

      return acc { extQues = (id, QueInput (QueueIn { queue   = arr
                                                    , quePtr  = ptr
                                                    , arrType = t })
                             ) : extQues acc
                 }
                   
    mkQueInput_ :: C.Type a -> [a] -> S.SBVCodeGen [S.SBV a]
    mkQueInput_ t que = do
      W.SymWordInst        <- return (W.symWordInst t)
      W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
      arr <- S.cgInputArr (length que) (mkQueueVar id)
      return arr

-----------------------------------------

mkExtInput_ :: C.Type a -> String -> S.SBVCodeGen (S.SBV a)
mkExtInput_ t name = do
  W.SymWordInst        <- return (W.symWordInst t)
  W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
  ext <- S.cgInput name
  return ext
