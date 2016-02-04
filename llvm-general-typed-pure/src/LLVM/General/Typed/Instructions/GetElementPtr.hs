{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LLVM.General.Typed.Instructions.GetElementPtr
  ( Bounds(..)
  , getElementPtr
  , getElementPtr0
  , GetElementPtrTest(..)
  , tryGetElementPtr
  , unsafeGetElementPtr
  , ElementIndex
  , GetElementPtrConstness
  , GetElementIndex
  , GetElementPtrType
  , Index(..)
  , InvalidGetElementPtrIndexBoundsPtr
  , InvalidGetElementPtrIndexBoundsStruct
  ) where

import Control.Monad.RWS.Lazy
import Data.Proxy
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as Semigroup
import Data.Void
import Foreign.Ptr (Ptr)
import GHC.Generics
import GHC.TypeLits
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueJoin
import LLVM.General.Typed.ValueOf
import LLVM.General.Typed.ValueSelect

data Bounds
  = InBounds
  | OutOfBounds
    deriving (Eq, Ord, Show)

instance Semigroup Bounds where
  InBounds <> InBounds = InBounds
  _        <> _        = OutOfBounds

data ElementIndex
  = ConstantElementIndex [Constant.Constant]
  | OperandElementIndex [AST.Operand]

instance Monoid ElementIndex where
  mempty = ConstantElementIndex []
  ConstantElementIndex xs `mappend` ConstantElementIndex ys = ConstantElementIndex $ xs <> ys
  ConstantElementIndex xs `mappend` OperandElementIndex ys  = OperandElementIndex $ fmap AST.ConstantOperand xs <> ys
  OperandElementIndex xs  `mappend` ConstantElementIndex ys = OperandElementIndex $ xs <> fmap AST.ConstantOperand ys
  OperandElementIndex xs  `mappend` OperandElementIndex ys  = OperandElementIndex $ xs <> ys

class GetElementIndex a i where
  type GetElementPtrType a i :: *
  getElementIndex :: proxy a -> i -> BasicBlock ElementIndex

-- |
-- Proving the result type of a 'getElementPtr' can be tedious or even impossible.
-- The preferred way to do these tests is using 'tryGetElementPtr' if the result is
-- an instance of 'Typeable'. Use this function with care.
unsafeGetElementPtr
  :: forall a b const i
   . GetElementIndex a i
  => ValueSelect const (GetElementPtrConstness const i)
  => ValueOf b
  => Bounds -- ^ Sum of all index 'Bounds'
  -> Value const a
  -> i
  -> BasicBlock (Value (GetElementPtrConstness const i) b)
unsafeGetElementPtr bounds value index = do
  elementIdx <- getElementIndex (Proxy :: Proxy a) index
  let inbounds = case bounds of InBounds -> True; OutOfBounds -> False
      ty = valueType (Proxy :: Proxy b)
  case elementIdx of
    OperandElementIndex idx -> vjoin (vselect f g value) where
      f _ = error "This should be unreachable, GEP on a mutable value is mutable"
      g x = nameInstruction ty $ AST.GetElementPtr inbounds x idx []
    ConstantElementIndex idx -> vjoin (vselect f g value) where
      f y = Constant.GetElementPtr inbounds y idx
      g x = nameInstruction ty $ AST.GetElementPtr inbounds x (fmap AST.ConstantOperand idx) []

data GetElementPtrTest a
  = GetElementPtrTypeMatch a   -- ^ The result type matches.
  | GetElementPtrTypeMismatch  -- ^ The result type does not match.
  | GetElementPtrTypeUnknown a -- ^ Indexing through an opaque type, this is unsafe.

-- |
-- Attempt to check the type of a 'getElementPtr' at runtime.
-- Note: this is currently not implemented and always returns
-- 'GetElementPtrTypeUnknown' even when the types match.
tryGetElementPtr
  :: ValueOf b
  => ValueSelect const (GetElementPtrConstness const i)
  => GetElementIndex a i
  => Bounds -- ^ Sum of all index 'Bounds'
  -> Value const a
  -> i
  -> BasicBlock (GetElementPtrTest (Value (GetElementPtrConstness const i) b))
tryGetElementPtr bounds value index =
  GetElementPtrTypeUnknown <$> unsafeGetElementPtr bounds value index

-- |
-- Calculate the 'Constness' of the 'Value' returned from 'getElementPtr'
type family GetElementPtrConstness (const :: Constness) (i :: *) :: Constness where
  GetElementPtrConstness 'Operand i = 'Operand
  GetElementPtrConstness 'Constant (Proxy Nat) = 'Constant
  GetElementPtrConstness 'Constant (Proxy [Nat]) = 'Constant
  GetElementPtrConstness 'Constant (Index i) = GGetElementPtrConstness 'Constant (Rep i)

-- |
-- Equivalent to 'GetElementPtrConstness' for '* -> *' kinds.
-- GHC seems to prefer this over a polykinded type family
type family GGetElementPtrConstness (const :: Constness) (i :: * -> *) :: Constness where
  GGetElementPtrConstness a (M1 i c f) = GGetElementPtrConstness a f
  GGetElementPtrConstness a (x :*: y) = GGetElementPtrConstness a x `Weakest` GGetElementPtrConstness a y
  GGetElementPtrConstness a (K1 i c) = GetElementPtrConstness a c

natElementIndex :: KnownNat n => proxy n -> ElementIndex
natElementIndex = ConstantElementIndex . (:[]) . Constant.Int 32 . natVal

valueElementIndex :: Value const a -> BasicBlock ElementIndex
valueElementIndex (ValueConstant x) = return $ ConstantElementIndex [x]
valueElementIndex (ValueWeakened x) = valueElementIndex x
valueElementIndex (ValuePure x)     = return $ OperandElementIndex [x]
valueElementIndex x@ValueOperand{}  = OperandElementIndex . (:[]) <$> asOp x

-- convienent names for testing
type InvalidGetElementPtrIndexBoundsPtr = Proxy "Attempting to index through a pointer"
type InvalidGetElementPtrIndexBoundsStruct = Proxy "Attempting to index past end of structure"

instance (KnownNat x, GetElementIndex a (Proxy xs)) => GetElementIndex (Ptr a) (proxy (x ': xs)) where
  type GetElementPtrType (Ptr a) (proxy (x ': xs)) = GetElementPtrType a (Proxy xs)
  getElementIndex _ _ = do
    xs <- getElementIndex (Proxy :: Proxy a) (Proxy :: Proxy xs)
    return $ natElementIndex (Proxy :: Proxy x) <> xs

instance KnownNat x => GetElementIndex (Ptr a) (proxy x) where
  type GetElementPtrType (Ptr a) (proxy x) = a
  getElementIndex _ _ = return $ natElementIndex (Proxy :: Proxy x)

instance GetElementIndex a ((proxy :: [Nat] -> *) '[]) where
  type GetElementPtrType a (proxy '[]) = a
  getElementIndex _ _ = return mempty

type family StructElement (a :: [*]) (n :: Nat) :: * where
  StructElement (x ': xs) 0 = x
  StructElement (x ': xs) n = StructElement xs (n - 1)
  StructElement '[] n = InvalidGetElementPtrIndexBoundsStruct

instance (KnownNat x, GetElementIndex (StructElement a x) (Proxy xs)) => GetElementIndex (Struct a) (proxy (x ': xs)) where
  type GetElementPtrType (Struct a) (proxy (x ': xs)) = GetElementPtrType (StructElement a x) (Proxy xs)
  getElementIndex _ _ = do
    xs <- getElementIndex (Proxy :: Proxy (StructElement a x)) (Proxy :: Proxy xs)
    return $ natElementIndex (Proxy :: Proxy x) <> xs

instance (KnownNat x, GetElementIndex a (Proxy xs), x <= n) => GetElementIndex (Array n a) (proxy (x ': xs)) where
  type GetElementPtrType (Array n a) (proxy (x ': xs)) = GetElementPtrType a (Proxy xs)
  getElementIndex _ _ = do
    xs <- getElementIndex (Proxy :: Proxy a) (Proxy :: Proxy xs)
    return $ natElementIndex (Proxy :: Proxy x) <> xs

instance GetElementIndex (Array n a) (Value const i) where
  type GetElementPtrType (Array n a) (Value const i) = a
  getElementIndex _ = valueElementIndex

instance GetElementIndex (Ptr a) (Value const i) where
  type GetElementPtrType (Ptr a) (Value const i) = a
  getElementIndex _ = valueElementIndex

newtype Index a = Index a

class GGetElementIndex a i where
  type GGetElementPtrType a i :: *
  ggetElementIndex :: proxy a -> i p -> BasicBlock ElementIndex

instance GGetElementIndex a f => GGetElementIndex a (M1 i c f) where
  type GGetElementPtrType a (M1 i c f) = GGetElementPtrType a f
  ggetElementIndex a (M1 f) = ggetElementIndex a f

instance (a ~ Proxy "Sum types are not supported by LLVM") => GGetElementIndex a (x :+: y) where
  type GGetElementPtrType a (x :+: y) = Void
  ggetElementIndex _ _ = error "Sum types are not supported by LLVM"

instance (a ~ Proxy "Uninhabited types are not supported by LLVM") => GGetElementIndex a V1 where
  type GGetElementPtrType a V1 = Void
  ggetElementIndex _ _ = error "Uninhabited types are not supported by LLVM"

instance (GGetElementIndex a x, GGetElementIndex (GGetElementPtrType a x) y) => GGetElementIndex a (x :*: y) where
  type GGetElementPtrType a (x :*: y) = GGetElementPtrType (GGetElementPtrType a x) y
  ggetElementIndex a (x :*: y) = do
    xs <- ggetElementIndex a x
    ys <- ggetElementIndex (Proxy :: Proxy (GGetElementPtrType a x)) y
    return $ xs <> ys

instance GetElementIndex a c => GGetElementIndex a (K1 i c) where
  type GGetElementPtrType a (K1 i c) = GetElementPtrType a c
  ggetElementIndex a (K1 c) = getElementIndex a c

instance (Generic idx, GGetElementIndex a (Rep idx)) => GetElementIndex a (Index idx) where
  type GetElementPtrType a (Index idx) = GGetElementPtrType a (Rep idx)
  getElementIndex a (Index idx) = ggetElementIndex a (from idx)

-- |
-- Following the conventions of LLVM's getelementptr instruction,
-- getElementPtr supports indexing into @'Value's@ of 'Ptr', 'Struct',
-- 'Array' and 'Vector'. Indexing into a 'Struct' requires a 'Nat'
-- proxy to ensure the result type is known. If all index elements
-- can be expressed as 'Nat' kinded types a promoted list can be used
-- instead of a tuple.
--
-- See: <http://llvm.org/docs/LangRef.html#getelementptr-instruction>
--
-- @
-- ('Proxy' :: 'Proxy' [0, 1, 2])
-- @
--
-- or a mix of @'Value's@ and @'Nat' -> *@ proxies can be specified as tuples:
--
-- @
-- (0 :: 'Value' 'Constant' 'Int32', 'Proxy' :: 'Proxy' 1, 2 :: 'Value' 'Constant' 'Int32')
-- @
getElementPtr
  :: forall a const index
   . ValueOf (GetElementPtrType (Ptr a) index)
  => ValueSelect const (GetElementPtrConstness const index)
  => GetElementIndex (Ptr a) index
  => Bounds -- ^ Sum of all index 'Bounds'
  -> Value const (Ptr a)
  -> index
  -> BasicBlock (Value (GetElementPtrConstness const index) (Ptr (GetElementPtrType (Ptr a) index)))
getElementPtr = unsafeGetElementPtr

type Index0 index = Index (Proxy 0, index)

-- |
--
getElementPtr0
  :: forall a const index
   . ValueOf (GetElementPtrType a index)
  => ValueSelect const (GetElementPtrConstness const (Index0 index))
  => GetElementIndex a index
  => Bounds -- ^ Sum of all index 'Bounds'
  -> Value const (Ptr a)
  -> index
  -> BasicBlock (Value (GetElementPtrConstness const (Index0 index)) (Ptr (GetElementPtrType (Ptr a) (Index0 index))))
getElementPtr0 bounds val index = getElementPtr bounds val (Index (Proxy :: Proxy 0, index))
