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
import qualified LLVM.General.AST.Constant as Constant
import qualified LLVM.General.AST.Float as Float

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

  valueOf :: FromConstant const => a -> Value (const :: Constness) a

instance ValueOf () where
  type BytesOf () = 0
  type ClassificationOf () = 'VoidClass
  valueType _ = AST.VoidType

instance ValueOf Bool where
  type BytesOf Bool = 1
  type ClassificationOf Bool = 'IntegerClass
  valueType _ = AST.IntegerType 8

instance ValueOf Int8 where
  type BytesOf Int8 = 1
  type ClassificationOf Int8 = 'IntegerClass
  valueType _ = AST.IntegerType 8

instance ValueOf Int16 where
  type BytesOf Int16 = 2
  type ClassificationOf Int16 = 'IntegerClass
  valueType _ = AST.IntegerType 16

instance ValueOf Int32 where
  type BytesOf Int32 = 4
  type ClassificationOf Int32 = 'IntegerClass
  valueType _ = AST.IntegerType 32

instance ValueOf Int64 where
  type BytesOf Int64 = 8
  type ClassificationOf Int64 = 'IntegerClass
  valueType _ = AST.IntegerType 64

instance ValueOf Word8 where
  type BytesOf Word8 = 1
  type ClassificationOf Word8 = 'IntegerClass
  valueType _ = AST.IntegerType 8

instance ValueOf Word16 where
  type BytesOf Word16 = 2
  type ClassificationOf Word16 = 'IntegerClass
  valueType _ = AST.IntegerType 16

instance ValueOf Word32 where
  type BytesOf Word32 = 4
  type ClassificationOf Word32 = 'IntegerClass
  valueType _ = AST.IntegerType 32

instance ValueOf Word64 where
  type BytesOf Word64 = 8
  type ClassificationOf Word64 = 'IntegerClass
  valueType _ = AST.IntegerType 64

instance ValueOf Float where
  type BytesOf Float = 4
  type ClassificationOf Float = 'FloatingPointClass
  valueType _ = AST.FloatingPointType 32 AST.IEEE

instance ValueOf Double where
  type BytesOf Double = 8
  type ClassificationOf Double = 'FloatingPointClass
  valueType _ = AST.FloatingPointType 64 AST.IEEE

instance ValueOf a => ValueOf (Ptr a) where
  type BytesOf (Ptr a) = SIZEOF_HSWORD
  type ClassificationOf (Ptr a) = 'PointerClass (ClassificationOf a)
  valueType _ = AST.PointerType (valueType (Proxy :: Proxy a)) (AST.AddrSpace 0)

instance (ValueOf a, KnownNat n) => ValueOf (Array n a) where
  type BytesOf (Array n a) = BytesOf a * n
  type ElementsOf (Array n a) = n
  type ClassificationOf (Array n a) = 'VectorClass (ClassificationOf a)
  valueType _ = AST.VectorType (fromInteger (natVal (Proxy :: Proxy n))) (valueType (Proxy :: Proxy a))

-- |
-- Internal class used to extract the fields of a structure as LLVM types
class FieldTypes a where
  fieldTypes :: proxy a -> [AST.Type]

instance (ValueOf x, FieldTypes (Struct xs)) => FieldTypes (Struct (x ': xs)) where
  fieldTypes _ = valueType (Proxy :: Proxy x) : fieldTypes (Proxy :: Proxy (Struct xs))

instance FieldTypes (Struct '[]) where
  fieldTypes _ = []

instance ValueOf (Struct '[]) where
  type BytesOf (Struct '[]) = 0
  type ClassificationOf (Struct '[]) = 'StructureClass
  valueType _ = AST.StructureType False []

instance (ValueOf x, ValueOf (Struct xs), FieldTypes (Struct (x ': xs))) => ValueOf (Struct (x ': xs)) where
  -- TODO: deal with padding
  type BytesOf (Struct (x ': xs)) = BytesOf x + BytesOf (Struct xs)
  type ClassificationOf (Struct (x ': xs)) = 'StructureClass
  valueType _ = AST.StructureType False (fieldTypes (Proxy :: Proxy (Struct (x ': xs))))
