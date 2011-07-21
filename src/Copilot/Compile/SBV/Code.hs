--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.SBV.Code
  ( updateStates
  , fireTriggers
  ) where

import Copilot.Compile.SBV.Copilot2SBV (c2sExpr)
import Copilot.Compile.SBV.MetaTable (MetaTable (..), StreamInfo (..)) 
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
    mkSBVFunc (mkUpdateStFn id) $
    do e' <- c2sExpr id meta e
       let Just strmInfo = M.lookup id (streamInfoMap meta)
       updateStreamState1 t1 e' strmInfo

  updateStreamState1 :: C.Type a -> S.SBV a -> StreamInfo -> S.SBVCodeGen ()
  updateStreamState1 t1 e1 (StreamInfo _ _ t2) = do
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
      e <- c2sExpr undefined meta guard
      S.cgReturn e 

  mkTriggerArg :: String -> (Int, C.TriggerArg) -> SBVFunc
  mkTriggerArg name (i, C.TriggerArg { C.triggerArgExpr = e
                                     , C.triggerArgType = t } ) = 
    mkSBVFunc (mkTriggerArgFn i name) mkExpr
    where  
    mkExpr = do
      e' <- c2sExpr undefined meta e
      W.SymWordInst <- return (W.symWordInst t)
      W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
      Just p <- return (t =~= t) 
      S.cgReturn $ coerce (cong p) e'
        
--------------------------------------------------------------------------------
