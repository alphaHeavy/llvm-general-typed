{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LLVM.General.Typed.ValueOf where

import Data.Int
import Data.Proxy
import Data.Word
import Foreign.Ptr (Ptr)
import GHC.TypeLits
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.AddrSpace as AST

import LLVM.General.Typed.Value

data Classification
  = VoidClass
  | IntegerClass
  | FloatingPointClass
  | PointerClass Classification
  | VectorClass Classification
  | StructureClass
  | LabelClass
  | MetadataClass

class ValueOf (a :: *) where
  type WordsOf a :: Nat
  type BitsOf a :: Nat
  type BitsOf a = WordsOf a * 8
  type ElementsOf a :: Nat
  type ElementsOf a = 1
  type ClassificationOf a :: Classification
  valueType :: proxy a -> AST.Type

instance ValueOf (Value const ()) where
  type WordsOf (Value const ()) = 0
  type ClassificationOf (Value const ()) = VoidClass
  valueType _ = AST.VoidType

instance ValueOf (Value const Bool) where
  type WordsOf (Value const Bool) = 1
  type ClassificationOf (Value const Bool) = IntegerClass
  valueType _ = AST.IntegerType 8

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

instance ValueOf (Value const a) => ValueOf (Value const (Ptr a)) where
  type WordsOf (Value const (Ptr a)) = 8 -- TODO: sizeof ptr
  type ClassificationOf (Value const (Ptr a)) = PointerClass (ClassificationOf (Value const a))
  valueType _ = AST.PointerType (valueType (Proxy :: Proxy (Value const a))) (AST.AddrSpace 0)

class StructureTypes a where
  structureTypes :: proxy a -> [AST.Type]

instance (ValueOf (Value const x), StructureTypes (Value const (Struct xs))) => StructureTypes (Value const (Struct (x ': xs))) where
  structureTypes _ = valueType (Proxy :: Proxy (Value const x)) : structureTypes (Proxy :: Proxy (Value const (Struct xs)))

instance StructureTypes (Value const (Struct '[])) where
  structureTypes _ = []

instance ValueOf (Value const (Struct '[])) where
  type WordsOf (Value const (Struct '[])) = 0
  type ClassificationOf (Value const (Struct '[])) = StructureClass
  valueType _ = AST.StructureType False []

instance (ValueOf (Value const x), ValueOf (Value const (Struct xs)), StructureTypes (Value const (Struct (x ': xs)))) =>
  ValueOf (Value const (Struct (x ': xs))) where
  type WordsOf (Value const (Struct (x ': xs))) = WordsOf (Value const x) + WordsOf (Value const (Struct xs))
  type ClassificationOf (Value const (Struct (x ': xs))) = StructureClass
  valueType _ = AST.StructureType False (structureTypes (Proxy :: Proxy (Value const (Struct (x ': xs)))))
