--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Compile.SBV.Copilot2SBV
  -- ( c2sExpr
  -- , c2sType
  -- ) 
where

import qualified Data.SBV as S
import qualified Data.SBV.Internals as S

import qualified Copilot.Compile.SBV.Queue as Q (lookahead)
import Copilot.Compile.SBV.MetaTable
import qualified Copilot.Compile.SBV.Witness as W
import qualified Copilot.Core as C
import Copilot.Core.Type.Equality ((=~=), coerce, cong)

import Data.Map (Map)
import qualified Data.Map as M
import Prelude hiding (id)

-- fib0 :: S.SWord64 -> S.SWord64
-- fib0 n = S.ite 
--            (n S..== 0 S.||| n S..== 1)
--            n
--            (fib0 (n-1) + fib0 (n-2))

-- foo :: S.SWord64
-- foo = S.literal 3 + 4
-- fib :: Stream Word64
-- fib = [1, 1] ++ fib + drop 1 fib

--------------------------------------------------------------------------------

c2sExpr :: MetaTable -> (forall e . C.Expr e => e a) -> S.SBVCodeGen (S.SBV a)
c2sExpr meta e = c2sExpr_ e M.empty meta

--------------------------------------------------------------------------------

-- c2SType :: C.Type a -> S.Type
-- c2SType t =
--   case t of
--     C.Bool   _ -> S.SBool
    -- C.Int8   _ -> A.Int8   ; C.Int16  _ -> A.Int16
    -- C.Int32  _ -> A.Int32  ; C.Int64  _ -> A.Int64
    -- C.Word8  _ -> A.Word8  ; C.Word16 _ -> A.Word16
    -- C.Word32 _ -> A.Word32 ; C.Word64 _ -> A.Word64
    -- C.Float  _ -> A.Float  ; C.Double _ -> A.Double

--------------------------------------------------------------------------------

data Local = forall a . Local
  { localSBVExpr :: S.SBV a
  , localType    :: C.Type a }

type Env = Map C.Name Local

--------------------------------------------------------------------------------

newtype C2SExpr a = C2SExpr
  { c2sExpr_ :: Env -> MetaTable -> S.SBVCodeGen (S.SBV a) }

newtype C2SOp1 a b = C2SOp1
  { c2sOp1 :: S.SBV a -> S.SBVCodeGen (S.SBV b) }

newtype C2SOp2 a b c = C2SOp2
  { c2sOp2 :: S.SBV a -> S.SBV b -> S.SBVCodeGen (S.SBV c) }

newtype C2SOp3 a b c d = C2SOp3
  { c2sOp3 :: S.SBV a -> S.SBV b -> S.SBV c -> S.SBVCodeGen (S.SBV d) }

--------------------------------------------------------------------------------

instance C.Expr C2SExpr where
  const t x = C2SExpr $ \ _ _ -> 
    case W.symWordInst t of W.SymWordInst -> return $ S.literal x

  ----------------------------------------------------

  drop t i id = C2SExpr $ \ _ meta ->
    let Just strmInfo = M.lookup id (streamInfoMap meta)
    in  drop1 t strmInfo
    where
    drop1 :: C.Type a -> StreamInfo -> S.SBVCodeGen (S.SBV a)
    drop1 t1
      StreamInfo
        { streamInfoQueue = que
        , streamInfoType  = t2
        } =
      let Just p = t2 =~= t1
      in  do W.SymWordInst <- return $ W.symWordInst t2
             W.HasSignAndSizeInst <- return $ W.hasSignAndSizeInst t2
             q   <- que
             idx <- Q.lookahead i q
             return $ coerce (cong p) idx

  ----------------------------------------------------

  local t1 _ name e1 e2 = C2SExpr $ \ env meta -> do
    e1'  <- c2sExpr_ e1 env meta
    let env' = M.insert name (Local e1' t1) env 
    c2sExpr_ e2 env' meta

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
            in  return $ coerce (cong p) e

  ----------------------------------------------------

  extern t name = C2SExpr $ \ _ _ -> do
    W.SymWordInst <- return (W.symWordInst t)
    W.HasSignAndSizeInst <- return (W.hasSignAndSizeInst t)
    S.cgInput name

  ----------------------------------------------------

  op1 op e = C2SExpr $ \ env meta -> do
    res1 <- c2sExpr_ e env meta
    c2sOp1 op res1

  ----------------------------------------------------

  op2 op e1 e2 = C2SExpr $ \ env meta -> do
    res1 <- c2sExpr_ e1 env meta
    res2 <- c2sExpr_ e2 env meta 
    c2sOp2 op res1 res2 

  ----------------------------------------------------

  op3 op e1 e2 e3 = C2SExpr $ \ env meta -> do
    res1 <- c2sExpr_ e1 env meta
    res2 <- c2sExpr_ e2 env meta 
    res3 <- c2sExpr_ e3 env meta
    c2sOp3 op res1 res2 res3
      
----------------------------------------------------

noFloatOpsErr :: String -> a
noFloatOpsErr op = 
  error ("Floating/Double operators not supported for the SBV backend: " 
         ++ "operator " ++ op ++ " not supported.")

----------------------------------------------------

eta1 :: (a -> a) -> (a -> S.SBVCodeGen a)
eta1 f = \a -> return $ f a

instance C.Op1 C2SOp1 where
  not     = C2SOp1 $                            eta1 S.bnot
  abs   t = C2SOp1 $ case W.symWordInst t of 
                       W.SymWordInst         -> eta1 abs 
  sign  t = C2SOp1 $ case W.symWordInst t of 
                       W.SymWordInst         -> eta1 signum
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

----------------------------------------------------

eta2 :: (a -> a -> a) -> (a -> a -> S.SBVCodeGen a)
eta2 f = \a b -> return $ f a b

eta2b :: (a -> a -> S.SBool) -> (a -> a -> S.SBVCodeGen (S.SBool))
eta2b f = \a b -> return $ f a b

instance C.Op2 C2SOp2 where
  and     = C2SOp2 $                                              eta2 (S.&&&)
  or      = C2SOp2 $                                              eta2 (S.|||)
  add   t = C2SOp2 $ case W.symWordInst  t of W.SymWordInst    -> eta2 (+)
  sub   t = C2SOp2 $ case W.symWordInst  t of W.SymWordInst    -> eta2 (-)
  mul   t = C2SOp2 $ case W.symWordInst  t of W.SymWordInst    -> eta2 (*)

  eq    t = C2SOp2 $ case W.eqInst       t of W.EqInst         -> eta2b (S..==)
  ne    t = C2SOp2 $ case W.eqInst       t of W.EqInst         -> eta2b (S../=)
  le    t = C2SOp2 $ case W.ordInst      t of W.OrdInst        -> eta2b (S..<=)
  ge    t = C2SOp2 $ case W.ordInst      t of W.OrdInst        -> eta2b (S..>=)
  lt    t = C2SOp2 $ case W.ordInst      t of W.OrdInst        -> eta2b (S..<)
  gt    t = C2SOp2 $ case W.ordInst      t of W.OrdInst        -> eta2b (S..>)

  div   t = C2SOp2 $ case W.polyInst     t of W.PolynomialInst -> eta2 (S.pDiv)
  mod   t = C2SOp2 $ case W.polyInst     t of W.PolynomialInst -> eta2 (S.pMod)

  fdiv  _ = noFloatOpsErr "fdiv"
  pow   _ = noFloatOpsErr "pow"
  logb  _ = noFloatOpsErr "logb"

instance C.Op3 C2SOp3 where
  mux t = C2SOp3 $ 
    case W.mergeableInst t of 
      W.MergeableInst -> \b c1 c2 -> return $ S.ite b c1 c2
                                  

----------------------------------------------------
