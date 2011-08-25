--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, GADTs #-}

module Copilot.Compile.SBV.Witness
  ( SymWordInst(..)       , symWordInst
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

data SymWordInst a = S.SymWord a => SymWordInst

symWordInst :: C.Type a -> SymWordInst a
symWordInst t =
  case t of
    C.Bool   -> SymWordInst
    C.Int8   -> SymWordInst ; C.Int16  -> SymWordInst
    C.Int32  -> SymWordInst ; C.Int64  -> SymWordInst
    C.Word8  -> SymWordInst ; C.Word16 -> SymWordInst
    C.Word32 -> SymWordInst ; C.Word64 -> SymWordInst
    C.Float  -> error "SymWordInst!" -- !! supress warning !!
    C.Double -> error "SymWordInst!" -- !! supress warning !!

--------------------------------------------------------------------------------

data HasSignAndSizeInst a = S.HasSignAndSize a => HasSignAndSizeInst

hasSignAndSizeInst :: C.Type a -> HasSignAndSizeInst a
hasSignAndSizeInst t =
  case t of
    C.Bool   -> HasSignAndSizeInst
    C.Int8   -> HasSignAndSizeInst 
    C.Int16  -> HasSignAndSizeInst
    C.Int32  -> HasSignAndSizeInst 
    C.Int64  -> HasSignAndSizeInst
    C.Word8  -> HasSignAndSizeInst 
    C.Word16 -> HasSignAndSizeInst
    C.Word32 -> HasSignAndSizeInst 
    C.Word64 -> HasSignAndSizeInst
    C.Float  -> error "HasSignAndSizeInst!" -- !! supress warning !!
    C.Double -> error "HasSignAndSizeInst!" -- !! supress warning !!

--------------------------------------------------------------------------------

data EqInst a = S.EqSymbolic (S.SBV a) => EqInst

eqInst :: C.Type a -> EqInst a
eqInst t =
  case t of
    C.Bool   -> EqInst
    C.Int8   -> EqInst ; C.Int16  -> EqInst
    C.Int32  -> EqInst ; C.Int64  -> EqInst
    C.Word8  -> EqInst ; C.Word16 -> EqInst
    C.Word32 -> EqInst ; C.Word64 -> EqInst
    C.Float  -> error "EqInst!" -- !! supress warning !!
    C.Double -> error "EqInst!" -- !! supress warning !!

--------------------------------------------------------------------------------

data BVDivisibleInst a = S.BVDivisible (S.SBV a) => BVDivisibleInst

divInst :: C.Type a -> BVDivisibleInst a
divInst t =
  case t of
    C.Bool   -> error "BVDivisibleInst!" -- !! supress warning !!
    C.Int8   -> error "BVDivisibleInst!" -- !! supress warning !!
    C.Int16  -> error "BVDivisibleInst!" -- !! supress warning !!
    C.Int32  -> error "BVDivisibleInst!" -- !! supress warning !!
    C.Int64  -> error "BVDivisibleInst!" -- !! supress warning !!
    C.Word8  -> BVDivisibleInst
    C.Word16 -> BVDivisibleInst
    C.Word32 -> BVDivisibleInst
    C.Word64 -> BVDivisibleInst
    C.Float  -> error "BVDivisibleInst!" -- !! supress warning !!
    C.Double -> error "BVDivisibleInst!" -- !! supress warning !!

--------------------------------------------------------------------------------

data OrdInst a = S.OrdSymbolic (S.SBV a) => OrdInst

ordInst :: C.Type a -> OrdInst a
ordInst t =
  case t of
    C.Bool   -> OrdInst
    C.Int8   -> OrdInst ; C.Int16  -> OrdInst
    C.Int32  -> OrdInst ; C.Int64  -> OrdInst
    C.Word8  -> OrdInst ; C.Word16 -> OrdInst
    C.Word32 -> OrdInst ; C.Word64 -> OrdInst
    C.Float  -> error "OrdInst!" -- !! supress warning !!
    C.Double -> error "OrdInst!" -- !! supress warning !!

--------------------------------------------------------------------------------

data MergeableInst a = S.Mergeable (S.SBV a) => MergeableInst

mergeableInst :: C.Type a -> MergeableInst a
mergeableInst t =
  case t of
    C.Bool   -> MergeableInst
    C.Int8   -> MergeableInst ; C.Int16  -> MergeableInst
    C.Int32  -> MergeableInst ; C.Int64  -> MergeableInst
    C.Word8  -> MergeableInst ; C.Word16 -> MergeableInst
    C.Word32 -> MergeableInst ; C.Word64 -> MergeableInst
    C.Float  -> error "MergeableInst!" -- !! supress warning !!
    C.Double -> error "MergeableInst!" -- !! supress warning !!

--------------------------------------------------------------------------------

data BitsInst a = (Bits a, S.Bits (S.SBV a)) => BitsInst

bitsInst :: C.Type a -> BitsInst a
bitsInst t =
  case t of
    C.Bool   -> error "BitsInst!" -- !! supress warning !!
    C.Int8   -> BitsInst ; C.Int16  -> BitsInst
    C.Int32  -> BitsInst ; C.Int64  -> BitsInst
    C.Word8  -> BitsInst ; C.Word16 -> BitsInst
    C.Word32 -> BitsInst ; C.Word64 -> BitsInst
    C.Float  -> error "BitsInst!" -- !! supress warning !!
    C.Double -> error "BitsInst!" -- !! supress warning !!

--------------------------------------------------------------------------------
