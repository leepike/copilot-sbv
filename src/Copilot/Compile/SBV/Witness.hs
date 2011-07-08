--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Copilot.Compile.SBV.Witness
  (
    SymWordInst(..)       , symWordInst
  , HasSignAndSizeInst(..), hasSignAndSizeInst
  , EqInst(..)            , eqInst 
  , PolynomialInst(..)    , polyInst
  , OrdInst(..)           , ordInst 
  , MergeableInst(..)     , mergeableInst 
  ) where

import qualified Data.SBV as S
import qualified Copilot.Core as C
import Copilot.Core.Type.Equality

--------------------------------------------------------------------------------

mkInst :: Equal a b -> f b -> f a
mkInst p con = coerce2 (symm p) con

--------------------------------------------------------------------------------

data SymWordInst a = S.SymWord a => SymWordInst

symWordInst :: C.Type a -> SymWordInst a
symWordInst t =
  case t of
    C.Bool   p -> mkInst p SymWordInst
    C.Int8   p -> mkInst p SymWordInst ; C.Int16  p -> mkInst p SymWordInst
    C.Int32  p -> mkInst p SymWordInst ; C.Int64  p -> mkInst p SymWordInst
    C.Word8  p -> mkInst p SymWordInst ; C.Word16 p -> mkInst p SymWordInst
    C.Word32 p -> mkInst p SymWordInst ; C.Word64 p -> mkInst p SymWordInst
    C.Float  _ -> error "SymWordInst!" -- !! supress warning !!
    C.Double _ -> error "SymWordInst!" -- !! supress warning !!

--------------------------------------------------------------------------------

data HasSignAndSizeInst a = S.HasSignAndSize a => HasSignAndSizeInst

hasSignAndSizeInst :: C.Type a -> HasSignAndSizeInst a
hasSignAndSizeInst t =
  case t of
    C.Bool   p -> mkInst p HasSignAndSizeInst
    C.Int8   p -> mkInst p HasSignAndSizeInst 
    C.Int16  p -> mkInst p HasSignAndSizeInst
    C.Int32  p -> mkInst p HasSignAndSizeInst 
    C.Int64  p -> mkInst p HasSignAndSizeInst
    C.Word8  p -> mkInst p HasSignAndSizeInst 
    C.Word16 p -> mkInst p HasSignAndSizeInst
    C.Word32 p -> mkInst p HasSignAndSizeInst 
    C.Word64 p -> mkInst p HasSignAndSizeInst
    C.Float  _ -> error "HasSignAndSizeInst!" -- !! supress warning !!
    C.Double _ -> error "HasSignAndSizeInst!" -- !! supress warning !!

--------------------------------------------------------------------------------

data EqInst a = S.EqSymbolic (S.SBV a) => EqInst

eqInst :: C.Type a -> EqInst a
eqInst t =
  case t of
    C.Bool   p -> mkInst p EqInst
    C.Int8   p -> mkInst p EqInst ; C.Int16  p -> mkInst p EqInst
    C.Int32  p -> mkInst p EqInst ; C.Int64  p -> mkInst p EqInst
    C.Word8  p -> mkInst p EqInst ; C.Word16 p -> mkInst p EqInst
    C.Word32 p -> mkInst p EqInst ; C.Word64 p -> mkInst p EqInst
    C.Float  _ -> error "EqInst!" -- !! supress warning !!
    C.Double _ -> error "EqInst!" -- !! supress warning !!

--------------------------------------------------------------------------------

data PolynomialInst a = S.Polynomial (S.SBV a) => PolynomialInst

polyInst :: C.Type a -> PolynomialInst a
polyInst t =
  case t of
    C.Bool   p -> error "PolynomialInst!" -- !! supress warning !!
    C.Int8   p -> error "PolynomialInst!" -- !! supress warning !!
    C.Int16  p -> error "PolynomialInst!" -- !! supress warning !!
    C.Int32  p -> error "PolynomialInst!" -- !! supress warning !!
    C.Int64  p -> error "PolynomialInst!" -- !! supress warning !!
    C.Word8  p -> mkInst p PolynomialInst
    C.Word16 p -> mkInst p PolynomialInst
    C.Word32 p -> mkInst p PolynomialInst
    C.Word64 p -> mkInst p PolynomialInst
    C.Float  _ -> error "PolynomialInst!" -- !! supress warning !!
    C.Double _ -> error "PolynomialInst!" -- !! supress warning !!

--------------------------------------------------------------------------------

data OrdInst a = S.OrdSymbolic (S.SBV a) => OrdInst

ordInst :: C.Type a -> OrdInst a
ordInst t =
  case t of
    C.Bool   p -> mkInst p OrdInst
    C.Int8   p -> mkInst p OrdInst ; C.Int16  p -> mkInst p OrdInst
    C.Int32  p -> mkInst p OrdInst ; C.Int64  p -> mkInst p OrdInst
    C.Word8  p -> mkInst p OrdInst ; C.Word16 p -> mkInst p OrdInst
    C.Word32 p -> mkInst p OrdInst ; C.Word64 p -> mkInst p OrdInst
    C.Float  _ -> error "OrdInst!" -- !! supress warning !!
    C.Double _ -> error "OrdInst!" -- !! supress warning !!

--------------------------------------------------------------------------------

data MergeableInst a = S.Mergeable (S.SBV a) => MergeableInst

mergeableInst :: C.Type a -> MergeableInst a
mergeableInst t =
  case t of
    C.Bool   p -> mkInst p MergeableInst
    C.Int8   p -> mkInst p MergeableInst ; C.Int16  p -> mkInst p MergeableInst
    C.Int32  p -> mkInst p MergeableInst ; C.Int64  p -> mkInst p MergeableInst
    C.Word8  p -> mkInst p MergeableInst ; C.Word16 p -> mkInst p MergeableInst
    C.Word32 p -> mkInst p MergeableInst ; C.Word64 p -> mkInst p MergeableInst
    C.Float  _ -> error "MergeableInst!" -- !! supress warning !!
    C.Double _ -> error "MergeableInst!" -- !! supress warning !!

--------------------------------------------------------------------------------



-- data Inst a = Inst

-- exprInst :: C.Type a -> Inst a
-- exprInst t =
--   case t of
--     C.Bool   p -> mkInst p Inst
--     C.Int8   p -> mkInst p Inst ; C.Int16  p -> mkInst p Inst
--     C.Int32  p -> mkInst p Inst ; C.Int64  p -> mkInst p Inst
--     C.Word8  p -> mkInst p Inst ; C.Word16 p -> mkInst p Inst
--     C.Word32 p -> mkInst p Inst ; C.Word64 p -> mkInst p Inst
--     C.Float  p -> mkInst p Inst ; C.Double p -> mkInst p Inst

-- --------------------------------------------------------------------------------

-- data AssignInst a = A.Assign a => AssignInst

-- assignInst :: C.Type a -> AssignInst a
-- assignInst t =
--   case t of
--     C.Bool   p -> mkInst p AssignInst
--     C.Int8   p -> mkInst p AssignInst ; C.Int16  p -> mkInst p AssignInst
--     C.Int32  p -> mkInst p AssignInst ; C.Int64  p -> mkInst p AssignInst
--     C.Word8  p -> mkInst p AssignInst ; C.Word16 p -> mkInst p AssignInst
--     C.Word32 p -> mkInst p AssignInst ; C.Word64 p -> mkInst p AssignInst
--     C.Float  p -> mkInst p AssignInst ; C.Double p -> mkInst p AssignInst

-- --------------------------------------------------------------------------------

-- data EqEInst a = A.EqE a => EqEInst

-- eqEInst :: Eq a => C.Type a -> EqEInst a
-- eqEInst t =
--   case t of
--     C.Bool   p -> mkInst p EqEInst
--     C.Int8   p -> mkInst p EqEInst ; C.Int16  p -> mkInst p EqEInst
--     C.Int32  p -> mkInst p EqEInst ; C.Int64  p -> mkInst p EqEInst
--     C.Word8  p -> mkInst p EqEInst ; C.Word16 p -> mkInst p EqEInst
--     C.Word32 p -> mkInst p EqEInst ; C.Word64 p -> mkInst p EqEInst
--     C.Float  p -> mkInst p EqEInst ; C.Double p -> mkInst p EqEInst

-- --------------------------------------------------------------------------------

-- data OrdEInst a = A.OrdE a => OrdEInst

-- ordEInst :: Ord a => C.Type a -> OrdEInst a
-- ordEInst t =
--   case t of
--     C.Bool   _ -> error "ordEInst!"
--     C.Int8   p -> mkInst p OrdEInst ; C.Int16  p -> mkInst p OrdEInst
--     C.Int32  p -> mkInst p OrdEInst ; C.Int64  p -> mkInst p OrdEInst
--     C.Word8  p -> mkInst p OrdEInst ; C.Word16 p -> mkInst p OrdEInst
--     C.Word32 p -> mkInst p OrdEInst ; C.Word64 p -> mkInst p OrdEInst
--     C.Float  p -> mkInst p OrdEInst ; C.Double p -> mkInst p OrdEInst


-- --------------------------------------------------------------------------------

-- data IntegralEInst a = A.IntegralE a => IntegralEInst

-- integralEInst :: Integral a => C.Type a -> IntegralEInst a
-- integralEInst t =
--   case t of
--     C.Bool   _ -> error "integralEInst!" -- !! supress warning !!
--     C.Int8   p -> mkInst p IntegralEInst ; C.Int16  p -> mkInst p IntegralEInst
--     C.Int32  p -> mkInst p IntegralEInst ; C.Int64  p -> mkInst p IntegralEInst
--     C.Word8  p -> mkInst p IntegralEInst ; C.Word16 p -> mkInst p IntegralEInst
--     C.Word32 p -> mkInst p IntegralEInst ; C.Word64 p -> mkInst p IntegralEInst
--     C.Float  _ -> error "integralEInst!" -- !! supress warning !!
--     C.Double _ -> error "integralEInst!" -- !! supress warning !!

-- --------------------------------------------------------------------------------

-- data FloatingEInst a = A.FloatingE a => FloatingEInst

-- floatingEInst :: Floating a => C.Type a -> FloatingEInst a
-- floatingEInst t =
--   case t of
--     C.Float  p -> mkInst p FloatingEInst
--     C.Double p -> mkInst p FloatingEInst
--     _          -> error "integralEInst!" -- !! supress warning !!

-- --------------------------------------------------------------------------------
