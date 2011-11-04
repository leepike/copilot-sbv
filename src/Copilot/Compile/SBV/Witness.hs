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

import Data.Bits

--------------------------------------------------------------------------------

badInst :: a
badInst = C.impossible "witnesses" "copilot-sbv"

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
    C.Float  -> badInst
    C.Double -> badInst

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
    C.Float  -> badInst
    C.Double -> badInst

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
    C.Float  -> badInst
    C.Double -> badInst

--------------------------------------------------------------------------------

data BVDivisibleInst a = S.BVDivisible (S.SBV a) => BVDivisibleInst

divInst :: C.Type a -> BVDivisibleInst a
divInst t =
  case t of
    C.Bool   -> badInst
    C.Int8   -> badInst
    C.Int16  -> badInst
    C.Int32  -> badInst
    C.Int64  -> badInst
    C.Word8  -> BVDivisibleInst
    C.Word16 -> BVDivisibleInst
    C.Word32 -> BVDivisibleInst
    C.Word64 -> BVDivisibleInst
    C.Float  -> badInst
    C.Double -> badInst

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
    C.Float  -> badInst
    C.Double -> badInst

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
    C.Float  -> badInst
    C.Double -> badInst

--------------------------------------------------------------------------------

data BitsInst a = (Bits a, S.Bits (S.SBV a)) => BitsInst

bitsInst :: C.Type a -> BitsInst a
bitsInst t =
  case t of
    C.Bool   -> badInst
    C.Int8   -> BitsInst ; C.Int16  -> BitsInst
    C.Int32  -> BitsInst ; C.Int64  -> BitsInst
    C.Word8  -> BitsInst ; C.Word16 -> BitsInst
    C.Word32 -> BitsInst ; C.Word64 -> BitsInst
    C.Float  -> badInst
    C.Double -> badInst

--------------------------------------------------------------------------------
