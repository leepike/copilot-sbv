--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}

module Copilot.Compile.SBV.Code
  ( updateStates
  , updateObservers
  , fireTriggers
  ) where

import Copilot.Compile.SBV.Copilot2SBV
import Copilot.Compile.SBV.MetaTable
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
                                                      } =
    mkSBVFunc (mkUpdateStFn id) $ do
      inputs <- mkInputs meta (c2Args e)
      let e' = c2sExpr inputs e
      let Just strmInfo = M.lookup id (streamInfoMap meta) 
      updateStreamState1 t1 e' strmInfo

  updateStreamState1 :: C.Type a -> S.SBV a -> StreamInfo -> S.SBVCodeGen ()
  updateStreamState1 t1 e1 (StreamInfo _ t2) = do
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

-- We first need to analyze the expression, running down it to get all the drop
-- ids and externals mentioned in it.  The we use those inputs to process the
-- expression.  XXX MUST be put in the monad in the same order as the function
-- call (argToCall from MetaTable.hs).

mkInputs :: MetaTable -> [Arg] -> S.SBVCodeGen [Input]
mkInputs meta args =
  mapM argToInput args

  where
  argToInput :: Arg -> S.SBVCodeGen Input
  argToInput (Extern name) = 
    let extInfos = externInfoMap meta in
    let Just extInfo = M.lookup name extInfos in
    mkExtInput extInfo

    where 
    mkExtInput :: C.UType -> S.SBVCodeGen Input
    mkExtInput C.UType { C.uTypeType = t } = do
      ext <- mkExtInput_ t
      return $ ExtIn name (ExtInput { extInput = ext 
                                    , extType  = t })

    mkExtInput_ :: C.Type a -> S.SBVCodeGen (S.SBV a)
    mkExtInput_ t = do
     W.SymWordInst        <- return (W.symWordInst t)
     W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
     ext <- S.cgInput (mkExtTmpVar name)
     return ext

  argToInput (Queue id) =
    let strmInfos = streamInfoMap meta in
    let Just strmInfo = M.lookup id strmInfos in
    mkArrInput strmInfo

    where
    mkArrInput :: StreamInfo -> S.SBVCodeGen Input
    mkArrInput StreamInfo { streamInfoQueue = que
                          , streamInfoType  = t } = do
      arr <- mkArrInput_ t que
      ptr <- S.cgInput (mkQueuePtrVar id)

      return $ ArrIn id (ArrInput (QueueIn { queue   = arr
                                           , quePtr  = ptr
                                           , arrType = t }))

    mkArrInput_ :: C.Type a -> [a] -> S.SBVCodeGen [S.SBV a]
    mkArrInput_ t que = do
      W.SymWordInst        <- return (W.symWordInst t)
      W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
      arr <- S.cgInputArr (length que) (mkQueueVar id)
      return arr
