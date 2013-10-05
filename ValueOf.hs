{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ValueOf where

import Data.Int
import Data.Word
import GHC.TypeLits
import qualified LLVM.General.AST as AST

import Value

class ValueOf (a :: *) where
  type WordsOf a :: Nat
  type BitsOf a :: Nat
  type BitsOf a = WordsOf a * 8
  type ElementsOf a :: Nat
  type ElementsOf a = 1
  type ClassificationOf a :: Classification
  valueType :: proxy a -> AST.Type

instance ValueOf (Value const Int8) where
  type WordsOf (Value const Int8) = 1
  type ClassificationOf (Value const Int8) = IntegerClass
  valueType _ = AST.IntegerType 8

instance ValueOf (Value const Int16) where
  type WordsOf (Value const Int16) = 2
  type ClassificationOf (Value const Int16) = IntegerClass
  valueType _ = AST.IntegerType 16

instance ValueOf (Value const Int32) where
  type WordsOf (Value const Int32) = 4
  type ClassificationOf (Value const Int32) = IntegerClass
  valueType _ = AST.IntegerType 32

instance ValueOf (Value const Int64) where
  type WordsOf (Value const Int64) = 8
  type ClassificationOf (Value const Int64) = IntegerClass
  valueType _ = AST.IntegerType 64

instance ValueOf (Value const Word8) where
  type WordsOf (Value const Word8) = 1
  type ClassificationOf (Value const Word8) = IntegerClass
  valueType _ = AST.IntegerType 8

instance ValueOf (Value const Word16) where
  type WordsOf (Value const Word16) = 2
  type ClassificationOf (Value const Word16) = IntegerClass
  valueType _ = AST.IntegerType 16

instance ValueOf (Value const Word32) where
  type WordsOf (Value const Word32) = 4
  type ClassificationOf (Value const Word32) = IntegerClass
  valueType _ = AST.IntegerType 32

instance ValueOf (Value const Word64) where
  type WordsOf (Value const Word64) = 8
  type ClassificationOf (Value const Word64) = IntegerClass
  valueType _ = AST.IntegerType 64

instance ValueOf (Value const Float) where
  type WordsOf (Value const Float) = 4
  type ClassificationOf (Value const Float) = FloatingPointClass
  valueType _ = AST.FloatingPointType 32 AST.IEEE

instance ValueOf (Value const Double) where
  type WordsOf (Value const Double) = 8
  type ClassificationOf (Value const Double) = FloatingPointClass
  valueType _ = AST.FloatingPointType 64 AST.IEEE
