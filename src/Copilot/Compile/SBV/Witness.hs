--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses #-}

module Copilot.Compile.SBV.Witness
  ( SymWordInst(..)       , symWordInst
  , NumInst(..)           , numInst
  , HasSignAndSizeInst(..), hasSignAndSizeInst
  , EqInst(..)            , eqInst 
  , CastInst(..)          , castInst   , sbvCast
  , BVDivisibleInst(..)   , divInst
  , OrdInst(..)           , ordInst 
  , MergeableInst(..)     , mergeableInst 
  , BitsInst(..)          , bitsInst 
  ) where

import qualified Data.SBV as S
import qualified Copilot.Core as C

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
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

data NumInst a = Num a => NumInst

numInst :: C.Type a -> NumInst a
numInst t =
  case t of
    C.Bool   -> badInst
    C.Int8   -> NumInst ; C.Int16  -> NumInst
    C.Int32  -> NumInst ; C.Int64  -> NumInst
    C.Word8  -> NumInst ; C.Word16 -> NumInst
    C.Word32 -> NumInst ; C.Word64 -> NumInst
    C.Float  -> badInst
    C.Double -> badInst

--------------------------------------------------------------------------------

data HasSignAndSizeInst a = S.SymWord a => HasSignAndSizeInst

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

data CastInst a b = SBVCast a b => CastInst

castInst :: C.Type a -> C.Type b -> CastInst a b
castInst t0 t1 = 
  case t0 of
    C.Bool   -> case t1 of
                  C.Bool    -> CastInst
                  C.Word8   -> CastInst
                  C.Word16  -> CastInst
                  C.Word32  -> CastInst
                  C.Word64  -> CastInst
                  C.Int8    -> CastInst
                  C.Int16   -> CastInst
                  C.Int32   -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst

    C.Word8  -> case t1 of
                  C.Word8   -> CastInst
                  C.Word16  -> CastInst
                  C.Word32  -> CastInst
                  C.Word64  -> CastInst
                  C.Int16   -> CastInst
                  C.Int32   -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst
    C.Word16 -> case t1 of
                  C.Word16  -> CastInst
                  C.Word32  -> CastInst
                  C.Word64  -> CastInst
                  C.Int32   -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst
    C.Word32 -> case t1 of
                  C.Word32  -> CastInst
                  C.Word64  -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst
    C.Word64 -> case t1 of
                  C.Word64  -> CastInst
                  _         -> badInst

    C.Int8   -> case t1 of
                  C.Int8    -> CastInst
                  C.Int16   -> CastInst
                  C.Int32   -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst
    C.Int16  -> case t1 of
                  C.Int16   -> CastInst
                  C.Int32   -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst
    C.Int32  -> case t1 of
                  C.Int32   -> CastInst
                  C.Int64   -> CastInst
                  _         -> badInst
    C.Int64  -> case t1 of
                  C.Int64   -> CastInst
                  _         -> badInst
    C.Float  -> badInst
    C.Double -> badInst

--------------------------------------------------------------------------------
-- | A class for casting SBV values.  We return errors for casts allowed by
-- Copilot.

class SBVCast a b where
  sbvCast :: S.SBV a -> S.SBV b

--------------------------------------------------------------------------------

castBool :: (Num a, S.SymWord a) => S.SBV Bool -> S.SBV a
castBool x = case S.unliteral x of
               Just bool -> if bool then 1 else 0
               Nothing   -> S.ite x 1 0

castErr :: a
castErr = C.badUsage $ "the SBV backend does not currently support casts to signed word types"

--------------------------------------------------------------------------------

instance SBVCast Bool Bool where
  sbvCast = id
instance SBVCast Bool Word8 where
  sbvCast = castBool
instance SBVCast Bool Word16 where
  sbvCast = castBool
instance SBVCast Bool Word32 where
  sbvCast = castBool
instance SBVCast Bool Word64 where
  sbvCast = castBool

instance SBVCast Bool Int8 where
  sbvCast = castErr 
instance SBVCast Bool Int16 where
  sbvCast = castErr 
instance SBVCast Bool Int32 where
  sbvCast = castErr 
instance SBVCast Bool Int64 where
  sbvCast = castErr 

--------------------------------------------------------------------------------

instance SBVCast Word8 Word8 where
  sbvCast = id
instance SBVCast Word8 Word16 where
  sbvCast = S.extend
instance SBVCast Word8 Word32 where
  sbvCast = S.extend . S.extend
instance SBVCast Word8 Word64 where
  sbvCast = S.extend . S.extend . S.extend

instance SBVCast Word8 Int16 where
  sbvCast = castErr 
instance SBVCast Word8 Int32 where
  sbvCast = castErr 
instance SBVCast Word8 Int64 where
  sbvCast = castErr 

--------------------------------------------------------------------------------

instance SBVCast Word16 Word16 where
  sbvCast = id
instance SBVCast Word16 Word32 where
  sbvCast = S.extend
instance SBVCast Word16 Word64 where
  sbvCast = S.extend . S.extend

instance SBVCast Word16 Int32 where
  sbvCast = castErr 
instance SBVCast Word16 Int64 where
  sbvCast = castErr 

--------------------------------------------------------------------------------

instance SBVCast Word32 Word32 where
  sbvCast = id
instance SBVCast Word32 Word64 where
  sbvCast = S.extend

instance SBVCast Word32 Int64 where
  sbvCast = castErr 

--------------------------------------------------------------------------------

instance SBVCast Word64 Word64 where
  sbvCast = id

--------------------------------------------------------------------------------

instance SBVCast Int8 Int8 where
  sbvCast = castErr 
instance SBVCast Int8 Int16 where
  sbvCast = castErr 
instance SBVCast Int8 Int32 where
  sbvCast = castErr 
instance SBVCast Int8 Int64 where
  sbvCast = castErr 

--------------------------------------------------------------------------------

instance SBVCast Int16 Int16 where
  sbvCast = castErr 
instance SBVCast Int16 Int32 where
  sbvCast = castErr 
instance SBVCast Int16 Int64 where
  sbvCast = castErr 

--------------------------------------------------------------------------------

instance SBVCast Int32 Int32 where
  sbvCast = castErr 
instance SBVCast Int32 Int64 where
  sbvCast = castErr 

--------------------------------------------------------------------------------

instance SBVCast Int64 Int64 where
  sbvCast = castErr 

--------------------------------------------------------------------------------

