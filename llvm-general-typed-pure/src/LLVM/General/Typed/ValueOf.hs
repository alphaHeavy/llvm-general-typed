{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#include "MachDeps.h"

module LLVM.General.Typed.ValueOf
  ( Classification(..)
  , ValueOf(..)
  , FieldTypes
  ) where

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

-- |
-- The bridge from Haskell types to LLVM types
class ValueOf (a :: *) where
  -- | The size of this type rounded up to the nearest byte
  type BytesOf a :: Nat

  -- | The size of this type in bits
  type BitsOf a :: Nat
  type BitsOf a = BytesOf a * 8

  -- | Number of elements represented by this type. This value only
  -- is useful for arrays, other types should use the default of 1.
  type ElementsOf a :: Nat
  type ElementsOf a = 1

  -- | Coarse grained classification of this type, useful for restricting
  -- what operands can be passed to certain instructions.
  type ClassificationOf a :: Classification
  valueType :: proxy a -> AST.Type

instance ValueOf (Value const ()) where
  type BytesOf (Value const ()) = 0
  type ClassificationOf (Value const ()) = 'VoidClass
  valueType _ = AST.VoidType

instance ValueOf (Value const Bool) where
  type BytesOf (Value const Bool) = 1
  type ClassificationOf (Value const Bool) = 'IntegerClass
  valueType _ = AST.IntegerType 8

instance ValueOf (Value const Int8) where
  type BytesOf (Value const Int8) = 1
  type ClassificationOf (Value const Int8) = 'IntegerClass
  valueType _ = AST.IntegerType 8

instance ValueOf (Value const Int16) where
  type BytesOf (Value const Int16) = 2
  type ClassificationOf (Value const Int16) = 'IntegerClass
  valueType _ = AST.IntegerType 16

instance ValueOf (Value const Int32) where
  type BytesOf (Value const Int32) = 4
  type ClassificationOf (Value const Int32) = 'IntegerClass
  valueType _ = AST.IntegerType 32

instance ValueOf (Value const Int64) where
  type BytesOf (Value const Int64) = 8
  type ClassificationOf (Value const Int64) = 'IntegerClass
  valueType _ = AST.IntegerType 64

instance ValueOf (Value const Word8) where
  type BytesOf (Value const Word8) = 1
  type ClassificationOf (Value const Word8) = 'IntegerClass
  valueType _ = AST.IntegerType 8

instance ValueOf (Value const Word16) where
  type BytesOf (Value const Word16) = 2
  type ClassificationOf (Value const Word16) = 'IntegerClass
  valueType _ = AST.IntegerType 16

instance ValueOf (Value const Word32) where
  type BytesOf (Value const Word32) = 4
  type ClassificationOf (Value const Word32) = 'IntegerClass
  valueType _ = AST.IntegerType 32

instance ValueOf (Value const Word64) where
  type BytesOf (Value const Word64) = 8
  type ClassificationOf (Value const Word64) = 'IntegerClass
  valueType _ = AST.IntegerType 64

instance ValueOf (Value const Float) where
  type BytesOf (Value const Float) = 4
  type ClassificationOf (Value const Float) = 'FloatingPointClass
  valueType _ = AST.FloatingPointType 32 AST.IEEE

instance ValueOf (Value const Double) where
  type BytesOf (Value const Double) = 8
  type ClassificationOf (Value const Double) = 'FloatingPointClass
  valueType _ = AST.FloatingPointType 64 AST.IEEE

instance ValueOf (Value const a) => ValueOf (Value const (Ptr a)) where
  type BytesOf (Value const (Ptr a)) = SIZEOF_HSWORD
  type ClassificationOf (Value const (Ptr a)) = 'PointerClass (ClassificationOf (Value const a))
  valueType _ = AST.PointerType (valueType (Proxy :: Proxy (Value const a))) (AST.AddrSpace 0)

instance (ValueOf (Value const a), KnownNat n) => ValueOf (Value const (Array n a)) where
  type BytesOf (Value const (Array n a)) = BytesOf (Value const a) * n
  type ElementsOf (Value const (Array n a)) = n
  type ClassificationOf (Value const (Array n a)) = 'VectorClass (ClassificationOf (Value const a))
  valueType _ = AST.VectorType (fromInteger (natVal (Proxy :: Proxy n))) (valueType (Proxy :: Proxy (Value const a)))

-- |
-- Internal class used to extract the fields of a structure as LLVM types
class FieldTypes a where
  fieldTypes :: proxy a -> [AST.Type]

instance (ValueOf (Value const x), FieldTypes (Value const (Struct xs))) => FieldTypes (Value const (Struct (x ': xs))) where
  fieldTypes _ = valueType (Proxy :: Proxy (Value const x)) : fieldTypes (Proxy :: Proxy (Value const (Struct xs)))

instance FieldTypes (Value const (Struct '[])) where
  fieldTypes _ = []

instance ValueOf (Value const (Struct '[])) where
  type BytesOf (Value const (Struct '[])) = 0
  type ClassificationOf (Value const (Struct '[])) = 'StructureClass
  valueType _ = AST.StructureType False []

instance (ValueOf (Value const x), ValueOf (Value const (Struct xs)), FieldTypes (Value const (Struct (x ': xs)))) =>
  ValueOf (Value const (Struct (x ': xs))) where
  -- TODO: deal with padding
  type BytesOf (Value const (Struct (x ': xs))) = BytesOf (Value const x) + BytesOf (Value const (Struct xs))
  type ClassificationOf (Value const (Struct (x ': xs))) = 'StructureClass
  valueType _ = AST.StructureType False (fieldTypes (Proxy :: Proxy (Value const (Struct (x ': xs)))))
