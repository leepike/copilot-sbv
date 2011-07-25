--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}

module Copilot.Compile.SBV.Code
  ( updateStates
  , fireTriggers
  ) where

import Copilot.Compile.SBV.Copilot2SBV
import Copilot.Compile.SBV.MetaTable 
import qualified Copilot.Compile.SBV.Queue as Q
--  (MetaTable(..), StreamInfo(..), c2Args, Arg(..)) 
import qualified Copilot.Compile.SBV.Witness as W
import Copilot.Compile.SBV.Common

import qualified Copilot.Core as C
import Copilot.Core.Type.Equality ((=~=), coerce, cong)

import qualified Data.SBV as S
import qualified Data.SBV.Internals as S

import qualified Data.Map as M
--import qualified Data.Maybe as N
--import qualified Data.List as L
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

  mkTriggerArg :: String -> (Int, C.TriggerArg) -> SBVFunc
  mkTriggerArg name (i, C.TriggerArg { C.triggerArgExpr = e
                                     , C.triggerArgType = t } ) = 
    mkSBVFunc (mkTriggerArgFn i name) mkExpr
    where  
    mkExpr = do
      inputs <- mkInputs meta (c2Args e)
      let e' = c2sExpr inputs e 
      W.SymWordInst <- return (W.symWordInst t)
      W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
      Just p <- return (t =~= t) 
      S.cgReturn $ coerce (cong p) e'
        
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
    mkExtInput :: ExternInfo -> S.SBVCodeGen Input
    mkExtInput ExternInfo { externInfoType = t } = do
      ext <- mkExtInput_ t
      return $ ExtIn name (ExtInput { extInput = ext 
                                    , extType  = t })

    mkExtInput_ :: C.Type a -> S.SBVCodeGen (S.SBV a)
    mkExtInput_ t = do
     W.SymWordInst        <- return (W.symWordInst t)
     W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
     ext <- S.cgInput (mkExtTmpVar name)
     Just p <- return (t =~= t)
     return $ coerce (cong p) ext

  argToInput (Queue id)    = 
    let strmInfos = streamInfoMap meta in
    let Just strmInfo = M.lookup id strmInfos in
    mkArrInput strmInfo 

    where
    mkArrInput :: StreamInfo -> S.SBVCodeGen Input
    mkArrInput StreamInfo { streamInfoQueue = que
                          , streamInfoType  = t } = do
      arr <- mkArrInput_ t que
      ptr <- mkPtrInput 
      return $ ArrIn id (ArrInput (QueueIn { queue   = arr
                                           , quePtr  = ptr 
                                           , arrType = t }))

    mkArrInput_ :: C.Type a -> [a] -> S.SBVCodeGen [S.SBV a]
    mkArrInput_ t que = do 
      W.SymWordInst        <- return (W.symWordInst t)
      W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
      arr <- S.cgInputArr (length que) (mkQueueVar id)
      Just p <- return (t =~= t)
      return $ map (coerce $ cong p) arr

    mkPtrInput :: S.SBVCodeGen (S.SBV Q.QueueSize)
    mkPtrInput = do 
      S.cgInput (mkQueuePtrVar id)

--    return ()


    -- getType :: forall a. StreamInfo -> C.Type a
    -- getType StreamInfo { streamInfoType  = t } = t 


  --   arr <- S.cgInputArr sz (mkQueueVar id)
  -- S.cgInput (mkQueuePtrVar id) in


  -- buf <- bufM
  -- return $

  -- where 
  -- bufM = do 
  --   W.SymWordInst        <- return (W.symWordInst t)
  --   W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
  --   arr <- S.cgInputArr sz (mkQueueVar id)
  --   Just p_ <- return (t =~= t)
  --   return $ map (coerce (cong p_)) arr

  -- -- get queue and queueIdxs
  -- strmInfosM :: [(C.Id, Maybe StreamInfo)]
  -- strmInfosM = 
  --   map (\id -> (id, M.lookup id (streamInfoMap meta)))
  --       (expDrops e) 

  -- strmInfos :: [(C.Id, Maybe StreamInfo)] -> [(C.Id, StreamInfo)]
  -- strmInfos = L.filter (\(_, m) -> m /= Nothing)

  -- getQueue :: StreamInfo -> [S.SBV a]
  -- getQueue StreamInfo { streamInfoQueue = que } = 
  --   map S.literal que

  -- exts = extNames e

--------------------------------------------------------------------------------

-- newtype DropIds a = DropIds
--   { expDrops :: [C.Id] }

-- instance C.Expr DropIds where
--   const _ _ = DropIds [] 
--   drop _ _ id = DropIds [id]
--   local _ _ _ e1 e2 = 
--     DropIds $ expDrops e1 ++ expDrops e2
--   var _ _ = DropIds []
--   extern _ _ = DropIds []
--   op1 _ e = DropIds (expDrops e)
--   op2 _ e1 e2 = 
--     DropIds $ expDrops e1 ++ expDrops e2
--   op3 _ e1 e2 e3 = 
--     DropIds $ expDrops e1 ++ expDrops e2 ++ expDrops e3

-- --------------------------------------------------------------------------------

-- newtype ExternNames a = ExternNames
--   { extNames :: [String] }

-- instance C.Expr ExternNames where
--   const _ _ = ExternNames [] 
--   drop _ _ _ = ExternNames []
--   local _ _ _ e1 e2 = 
--     ExternNames $ extNames e1 ++ extNames e2
--   var _ _ = ExternNames []
--   extern _ name = ExternNames [name]
--   op1 _ e = ExternNames (extNames e)
--   op2 _ e1 e2 = 
--     ExternNames $ extNames e1 ++ extNames e2
--   op3 _ e1 e2 e3 = 
--     ExternNames $ extNames e1 ++ extNames e2 ++ extNames e3
