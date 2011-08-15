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
  , BVDivisibleInst(..)   , divInst
  , OrdInst(..)           , ordInst 
  , MergeableInst(..)     , mergeableInst 
  , BitsInst(..)          , bitsInst 
  ) where

import qualified Data.SBV as S
import qualified Data.SBV.Internals as S
import qualified Copilot.Core as C
import Copilot.Core.Type.Equality

import Data.Bits
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

data BVDivisibleInst a = S.BVDivisible (S.SBV a) => BVDivisibleInst

divInst :: C.Type a -> BVDivisibleInst a
divInst t =
  case t of
    C.Bool   _ -> error "BVDivisibleInst!" -- !! supress warning !!
    C.Int8   _ -> error "BVDivisibleInst!" -- !! supress warning !!
    C.Int16  _ -> error "BVDivisibleInst!" -- !! supress warning !!
    C.Int32  _ -> error "BVDivisibleInst!" -- !! supress warning !!
    C.Int64  _ -> error "BVDivisibleInst!" -- !! supress warning !!
    C.Word8  p -> mkInst p BVDivisibleInst
    C.Word16 p -> mkInst p BVDivisibleInst
    C.Word32 p -> mkInst p BVDivisibleInst
    C.Word64 p -> mkInst p BVDivisibleInst
    C.Float  _ -> error "BVDivisibleInst!" -- !! supress warning !!
    C.Double _ -> error "BVDivisibleInst!" -- !! supress warning !!

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

data BitsInst a = (Bits a, S.Bits (S.SBV a)) => BitsInst

bitsInst :: C.Type a -> BitsInst a
bitsInst t =
  case t of
    C.Bool   _ -> error "BitsInst!" -- !! supress warning !!
    C.Int8   p -> mkInst p BitsInst ; C.Int16  p -> mkInst p BitsInst
    C.Int32  p -> mkInst p BitsInst ; C.Int64  p -> mkInst p BitsInst
    C.Word8  p -> mkInst p BitsInst ; C.Word16 p -> mkInst p BitsInst
    C.Word32 p -> mkInst p BitsInst ; C.Word64 p -> mkInst p BitsInst
    C.Float  _ -> error "BitsInst!" -- !! supress warning !!
    C.Double _ -> error "BitsInst!" -- !! supress warning !!

--------------------------------------------------------------------------------
