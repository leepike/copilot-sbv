--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Compile.SBV.Copilot2SBV
  ( c2sExpr
  , Input(..)
  , ExtInput(..)
  , ArrInput(..)
  , QueueIn(..)
  ) 
where

import Prelude hiding (id)
import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.SBV as S
import qualified Data.SBV.Internals as S

import qualified Copilot.Compile.SBV.Queue as Q
import qualified Copilot.Compile.SBV.Witness as W

import qualified Copilot.Core as C
import Copilot.Core.Type.Equality ((=~=), coerce, cong)


--------------------------------------------------------------------------------

data Input = 
    ExtIn C.Name ExtInput
  | ArrIn C.Id ArrInput

data ExtInput = forall a. ExtInput 
  { extInput :: S.SBV a
  , extType  :: C.Type a }

data ArrInput = forall a. ArrInput 
  { arrInput :: QueueIn a }

data QueueIn a = QueueIn
  { queue  :: [S.SBV a]
  , quePtr :: S.SBV Q.QueueSize 
  , arrType  :: C.Type a }

--------------------------------------------------------------------------------

c2sExpr :: [Input] -> (forall e. C.Expr e => e a) -> S.SBV a
c2sExpr inputs e = 
  c2sExpr_ e M.empty inputs

--------------------------------------------------------------------------------

data Local = forall a . Local
  { localSBVExpr :: S.SBV a
  , localType    :: C.Type a }

type Env = Map C.Name Local

--------------------------------------------------------------------------------

newtype C2SExpr a = C2SExpr
  { c2sExpr_ :: Env -> [Input] -> S.SBV a }

newtype C2SOp1 a b = C2SOp1
  { c2sOp1 :: S.SBV a -> S.SBV b }

newtype C2SOp2 a b c = C2SOp2
  { c2sOp2 :: S.SBV a -> S.SBV b -> S.SBV c }

newtype C2SOp3 a b c d = C2SOp3
  { c2sOp3 :: S.SBV a -> S.SBV b -> S.SBV c -> S.SBV d }

--------------------------------------------------------------------------------

instance C.Expr C2SExpr where
  const t x = C2SExpr $ \ _ _ -> 
    case W.symWordInst t of W.SymWordInst -> S.literal x

  ----------------------------------------------------

  drop t i id = C2SExpr $ \ _ inputs ->
    let que :: ArrInput
        Just que = foldl 
          ( \acc x -> case x of
                        ArrIn id' q -> if id' == id then Just q 
                                         else acc
                        ExtIn _ _ -> acc ) 
          Nothing
          inputs 
    in 
    drop1 t que

    where
    drop1 :: C.Type a -> ArrInput -> S.SBV a
    drop1 t1 ArrInput { arrInput = QueueIn { queue   = que 
                                           , quePtr  = qPtr
                                           , arrType = t2 } } =
      let Just p = t2 =~= t1 in
      case W.symWordInst t2 of
        W.SymWordInst -> 
          case W.hasSignAndSizeInst t2 of
            W.HasSignAndSizeInst ->
              coerce (cong p) (Q.lookahead i que qPtr)

  ----------------------------------------------------

  local t1 _ name e1 e2 = C2SExpr $ \ env inputs -> 
    let e1' = c2sExpr_ e1 env inputs in
    let env' = M.insert name (Local e1' t1) env in
    c2sExpr_ e2 env' inputs

  ----------------------------------------------------

  var t1 name = C2SExpr $ \ env _ ->
    let Just local = M.lookup name env
    in
      case local of
        Local
          { localSBVExpr = e
          , localType    = t2
          } ->
            let Just p = t2 =~= t1
            in  coerce (cong p) e

  ----------------------------------------------------

  externVar t name = C2SExpr $ \ _ inputs -> 
    let ext :: ExtInput
        Just ext = foldl 
          ( \acc x -> case x of
                        ArrIn _ _ -> acc
                        ExtIn nm e -> if nm == name then Just e
                                        else acc ) 
          Nothing
          inputs 
    in getSBV t ext

    where 
    getSBV :: C.Type a -> ExtInput -> S.SBV a
    getSBV t1 ExtInput { extInput = ext
                       , extType  = t2 } =
      let Just p = t2 =~= t1 in
      coerce (cong p) ext

  ----------------------------------------------------

  -- externFun t name _ = C2AExpr $ \ _ meta ->
  --   let Just extFunInfo = M.lookup name (externFunInfoMap meta) in
  --   externFun1 t extFunInfo

  --   where
  --   externFun1 t1
  --     ExternFunInfo
  --       { externFunInfoVar  = var
  --       , externFunInfoType = t2
  --       } =
  --     let Just p = t2 =~= t1 in
  --     case W.exprInst t2 of
  --       W.ExprInst -> coerce (cong p) (A.value var)
 
  ----------------------------------------------------

  op1 op e = C2SExpr $ \ env inputs -> 
    let res1 = c2sExpr_ e env inputs in
    c2sOp1 op res1

  ----------------------------------------------------

  op2 op e1 e2 = C2SExpr $ \ env inputs -> 
    let res1 = c2sExpr_ e1 env inputs in
    let res2 = c2sExpr_ e2 env inputs in
    c2sOp2 op res1 res2 

  ----------------------------------------------------

  op3 op e1 e2 e3 = C2SExpr $ \ env inputs -> 
    let res1 = c2sExpr_ e1 env inputs in
    let res2 = c2sExpr_ e2 env inputs in
    let res3 = c2sExpr_ e3 env inputs in
    c2sOp3 op res1 res2 res3

--------------------------------------------------------------------------------      

noFloatOpsErr :: String -> a
noFloatOpsErr op = 
  error ("Floating/Double operators not supported for the SBV backend: " 
         ++ "operator " ++ op ++ " not supported.")

--------------------------------------------------------------------------------      

-- eta1 :: (a -> a) -> (a -> S.SBVCodeGen a)
-- eta1 f = \a -> return $ f a

instance C.Op1 C2SOp1 where
  not     = C2SOp1 $ \x -> S.ite (x S..== S.false) S.true S.false
  abs   t = C2SOp1 $ case W.symWordInst t of 
                       W.SymWordInst         -> abs 
  sign  t = C2SOp1 $ case W.symWordInst t of 
                       W.SymWordInst         -> signum
  bwNot t = C2SOp1 $ case W.bitsInst    t of 
                       W.BitsInst            -> (S.complement)

  recip _ = noFloatOpsErr "recip"
  exp   _ = noFloatOpsErr "exp"
  sqrt  _ = noFloatOpsErr "sqrt"
  log   _ = noFloatOpsErr "log"
  sin   _ = noFloatOpsErr "sin"
  tan   _ = noFloatOpsErr "tan"
  cos   _ = noFloatOpsErr "cos"
  asin  _ = noFloatOpsErr "asin"
  atan  _ = noFloatOpsErr "atan"
  acos  _ = noFloatOpsErr "acos"
  sinh  _ = noFloatOpsErr "sinh"
  tanh  _ = noFloatOpsErr "tanh"
  cosh  _ = noFloatOpsErr "cosh"
  asinh _ = noFloatOpsErr "asinh"
  atanh _ = noFloatOpsErr "atanh"
  acosh _ = noFloatOpsErr "acosh"

--------------------------------------------------------------------------------

instance C.Op2 C2SOp2 where
  and     = C2SOp2 $ \x y -> S.ite (x S..== S.false) 
                                   S.false 
                                   (S.ite (y S..== S.false) S.false S.true)
  or      = C2SOp2 $ \x y -> S.ite (x S..== S.false) 
                                   (S.ite (y S..== S.false) S.false S.true)
                                   S.true
  add   t = C2SOp2 $ case W.symWordInst  t of W.SymWordInst    ->  (+)
  sub   t = C2SOp2 $ case W.symWordInst  t of W.SymWordInst    ->  (-)
  mul   t = C2SOp2 $ case W.symWordInst  t of W.SymWordInst    ->  (*)

  eq    t = C2SOp2 $ case W.eqInst       t of W.EqInst         ->  (S..==)
  ne    t = C2SOp2 $ case W.eqInst       t of W.EqInst         ->  (S../=)
  le    t = C2SOp2 $ case W.ordInst      t of W.OrdInst        ->  (S..<=)
  ge    t = C2SOp2 $ case W.ordInst      t of W.OrdInst        ->  (S..>=)
  lt    t = C2SOp2 $ case W.ordInst      t of W.OrdInst        ->  (S..<)
  gt    t = C2SOp2 $ case W.ordInst      t of W.OrdInst        ->  (S..>)

  div   t = C2SOp2 $ case W.divInst      t of W.BVDivisibleInst  ->  
                                                  \x y -> fst (S.bvQuotRem x y)
  mod   t = C2SOp2 $ case W.divInst      t of W.BVDivisibleInst  ->  
                                                  \x y -> snd (S.bvQuotRem x y)

  bwAnd t = C2SOp2 $ case W.bitsInst     t of W.BitsInst       -> (S..&.)
  bwOr  t = C2SOp2 $ case W.bitsInst     t of W.BitsInst       -> (S..|.)
  bwXor t = C2SOp2 $ case W.bitsInst     t of W.BitsInst       -> (S.xor)

  fdiv  _ = noFloatOpsErr "fdiv"
  pow   _ = noFloatOpsErr "pow"
  logb  _ = noFloatOpsErr "logb"

instance C.Op3 C2SOp3 where
  mux t = C2SOp3 $ 
    case W.mergeableInst t of 
      W.MergeableInst -> \b c1 c2 -> S.ite b c1 c2
                                  
--------------------------------------------------------------------------------      
