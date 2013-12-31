{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Instructions where

import Control.Applicative
import Control.Monad.RWS.Lazy
import Data.Proxy
import Data.Traversable
import Foreign.Ptr (Ptr)
import GHC.Generics
import GHC.TypeLits
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Attribute as Attribute
import qualified LLVM.General.AST.Constant as Constant
import qualified LLVM.General.AST.FloatingPointPredicate as FloatingPointPredicate
import qualified LLVM.General.AST.IntegerPredicate as IntegerPredicate

import AnyValue
import BasicBlock
import FreshName
import Function
import Value
import ValueOf
import VMap

ret
  :: ValueOf (Value const a)
  => Value const a
  -> BasicBlock (Terminator ())
ret value = do
  -- name the value, emitting instructions as necessary
  valueOp <- asOp value
  setTerminator $ AST.Ret (Just valueOp) []
  -- @TODO: replace with LocalReference ?
  return $ Terminator ()

ret_ :: BasicBlock (Terminator ())
ret_ = do
  setTerminator $ AST.Ret Nothing []
  return $ Terminator ()

condBr
  :: Value const Bool
  -> Label
  -> Label
  -> BasicBlock (Terminator ())
condBr condition (Label trueDest) (Label falseDest) = do
  conditionOp <- asOp condition
  setTerminator $ AST.CondBr conditionOp trueDest falseDest []
  return $ Terminator ()

br :: Label -> BasicBlock (Terminator ())
br (Label dest) = do
  setTerminator $ AST.Br dest []
  return $ Terminator ()

switch
  :: ( ClassificationOf (Value const a)     ~ IntegerClass,
       ClassificationOf (Value 'Constant a) ~ IntegerClass)
  => Value const a
  -> Label -- default
  -> [(Value 'Constant a, Label)]
  -> BasicBlock (Terminator ())
switch value (Label defaultDest) dests = do
  valueOp <- asOp value
  let dests' = [(val, dest) | (ValueConstant val, Label dest) <- dests]
  setTerminator $ AST.Switch valueOp defaultDest dests' []
  return $ Terminator ()

indirectBr = undefined

invoke = undefined

resume = undefined

unreachable
  :: BasicBlock (Terminator ())
unreachable = do
  setTerminator $ AST.Unreachable []
  return $ Terminator ()

undef
  :: forall a .
     ValueOf (Value 'Constant a)
  => BasicBlock (Value 'Constant a)
undef = do
  let val = Constant.Undef $ valueType (Proxy :: Proxy (Value 'Constant a))
  return $ ValueConstant val

class Phi (f :: * -> *) where
  phi :: ValueOf (Value 'Mutable a) => [(f a, Label)] -> BasicBlock (Value 'Mutable a)

instance Phi (Value const) where
  phi :: forall a . ValueOf (Value 'Mutable a) => [(Value const a, Label)] -> BasicBlock (Value 'Mutable a)
  phi incomingValues = do
    -- @TODO: make sure we have evaluated all of the values in the list...
    incomingValues' <- for incomingValues $ \ (val, Label origin) -> do
      valOp <- asOp val
      return (valOp, origin)

    let ty = valueType (Proxy :: Proxy (Value 'Mutable a))
    ValueOperand . return <$> nameInstruction (AST.Phi ty incomingValues' [])

instance Phi AnyValue where
  phi :: forall a . ValueOf (Value 'Mutable a) => [(AnyValue a, Label)] -> BasicBlock (Value 'Mutable a)
  phi incomingValues = do
    -- @TODO: make sure we have evaluated all of the values in the list...
    incomingValues' <- for incomingValues $ \ (AnyValue val, Label origin) -> do
      valOp <- asOp val
      return (valOp, origin)

    let ty = valueType (Proxy :: Proxy (Value 'Mutable a))
    ValueOperand . return <$> nameInstruction (AST.Phi ty incomingValues' [])

alloca
  :: forall a .
     ( ValueOf (Value 'Mutable a)
     , KnownNat (ElementsOf (Value 'Mutable a)))
  => BasicBlock (Value 'Mutable (Ptr a))
alloca = do
  let ty = valueType (Proxy :: Proxy (Value 'Mutable a))
      ne = natVal (Proxy :: Proxy (ElementsOf (Value 'Mutable a)))
  -- @TODO: the hardcoded 64 should probably be the target word size?
      inst = AST.Alloca ty (Just (AST.ConstantOperand (Constant.Int 64 ne))) 0 []
  ValueOperand . return <$> nameInstruction inst

load
  :: Value const (Ptr a)
  -> BasicBlock (Value 'Mutable a)
load x = do
  x' <- asOp x
  ValueOperand . return <$> nameInstruction (AST.Load False x' Nothing 0 [])

store
  :: Value cx (Ptr a)
  -> Value cy a
  -> BasicBlock ()
store address value = do
  address' <- asOp address
  value' <- asOp value
  let instr = AST.Store False address' value' Nothing 0 []
  tell [AST.Do instr]

{-
type family ResultType a :: *

class BundleArgs f where
  xxxx :: f -> BasicBlock [(AST.Operand, [Attribute.ParameterAttribute])]
  xxxx = undefined

call :: Function cconv ty -> args -> BasicBlock (ResultType ty)
call = error "call"
-}

data InBounds
  = InBounds
  | OutOfBounds
    deriving (Eq, Ord, Show)

class GetElementPtr a i where
  type GetElementPtrType a i :: *
  getElementIndex :: proxy a -> i -> BasicBlock [AST.Operand]

natOperand :: KnownNat n => proxy n -> AST.Operand
natOperand = AST.ConstantOperand . Constant.Int 32 . natVal

-- convienent names for testing
type InvalidGetElementPtrIndexBoundsPtr = Proxy "Attempting to index through a pointer"
type InvalidGetElementPtrIndexBoundsStruct = Proxy "Attempting to index past end of structure"

instance (KnownNat x, GetElementPtr a (Proxy xs)) => GetElementPtr (Ptr a) (proxy (x ': xs)) where
  type GetElementPtrType (Ptr a) (proxy (x ': xs)) = GetElementPtrType a (Proxy xs)
  getElementIndex _ _ = do
    xs <- getElementIndex (Proxy :: Proxy a) (Proxy :: Proxy xs)
    return $ natOperand (Proxy :: Proxy x) : xs

instance KnownNat x => GetElementPtr (Ptr a) (proxy x) where
  type GetElementPtrType (Ptr a) (proxy x) = a
  getElementIndex _ _ = return [natOperand (Proxy :: Proxy x)]

instance GetElementPtr a ((proxy :: [Nat] -> *) '[]) where
  type GetElementPtrType a (proxy '[]) = a
  getElementIndex _ _ = return []

type family StructElement (a :: [*]) (n :: Nat) :: * where
  StructElement (x ': xs) 0 = x
  StructElement (x ': xs) n = StructElement xs (n - 1)
  StructElement '[] n = InvalidGetElementPtrIndexBoundsStruct

instance (KnownNat x, GetElementPtr (StructElement a x) (Proxy xs)) => GetElementPtr (Struct a) (proxy (x ': xs)) where
  type GetElementPtrType (Struct a) (proxy (x ': xs)) = GetElementPtrType (StructElement a x) (Proxy xs)
  getElementIndex _ _ = do
    xs <- getElementIndex (Proxy :: Proxy (StructElement a x)) (Proxy :: Proxy xs)
    return $ natOperand (Proxy :: Proxy x) : xs

instance (KnownNat x, GetElementPtr a (Proxy xs), x <= n) => GetElementPtr (Array n a) (proxy (x ': xs)) where
  type GetElementPtrType (Array n a) (proxy (x ': xs)) = GetElementPtrType a (Proxy xs)
  getElementIndex _ _ = do
    xs <- getElementIndex (Proxy :: Proxy a) (Proxy :: Proxy xs)
    return $ natOperand (Proxy :: Proxy x) : xs

instance GetElementPtr (Array n a) (Value const i) where
  type GetElementPtrType (Array n a) (Value const i) = a
  getElementIndex _ = fmap (:[]) . asOp

instance GetElementPtr (Ptr a) (Value const i) where
  type GetElementPtrType (Ptr a) (Value const i) = a
  getElementIndex _ = fmap (:[]) . asOp

newtype Index a = Index a

class GGetElementPtr a i where
  type GGetElementPtrType a i :: *
  ggetElementIndex :: proxy a -> i p -> BasicBlock [AST.Operand]

instance GGetElementPtr a f => GGetElementPtr a (M1 i c f) where
  type GGetElementPtrType a (M1 i c f) = GGetElementPtrType a f
  ggetElementIndex a (M1 f) = ggetElementIndex a f

instance (GGetElementPtr a x, GGetElementPtr (GGetElementPtrType a x) y) => GGetElementPtr a (x :*: y) where
  type GGetElementPtrType a (x :*: y) = GGetElementPtrType (GGetElementPtrType a x) y
  ggetElementIndex a (x :*: y) = do
    xs <- ggetElementIndex a x
    ys <- ggetElementIndex (Proxy :: Proxy (GGetElementPtrType a x)) y
    return $ xs ++ ys

instance GetElementPtr a c => GGetElementPtr a (K1 i c) where
  type GGetElementPtrType a (K1 i c) = GetElementPtrType a c
  ggetElementIndex a (K1 c) = getElementIndex a c

instance (Generic idx, GGetElementPtr a (Rep idx)) => GetElementPtr a (Index idx) where
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
  :: forall a const index . (GetElementPtr (Ptr a) index, ValueJoin const)
  => InBounds
  -> Value const (Ptr a)
  -> index
  -> BasicBlock (Value const (Ptr (GetElementPtrType (Ptr a) index)))
getElementPtr bounds value indices = do
  idx <- getElementIndex (Proxy :: Proxy (Ptr a)) indices
  let inbounds = case bounds of InBounds -> True; OutOfBounds -> False
      f y = Constant.GetElementPtr inbounds y [error "damn"]
      g x = nameInstruction $ AST.GetElementPtr inbounds x idx []
  vmap1' f g value

{-
getElementPtr0
  :: forall a const i proxy . (GetElementPtr (Value const a) (Proxy 0 ': i), ValueJoin const)
  => InBounds
  -> Value const a
  -> proxy i
  -> BasicBlock (Value const (Ptr (GetElementPtrType a (Proxy 0 ': i))))
getElementPtr0 bounds val _ = getElementPtr bounds val (Proxy :: Proxy (Proxy 0 ': i))
-}

class Name (const :: Constness) where
  name :: Value const a -> BasicBlock (Value const a)

instance Name 'Constant where
  name = return

instance Name 'Mutable where
  name val = do
    n <- freshName
    undefined

{-
name :: String -> Value const a -> BasicBlock (Value const a)
name = undefined

name_ :: Value const a -> BasicBlock (Value const a)
name_ = undefined
-}

trunc
  :: forall a b const .
     ( ClassificationOf (Value const a) ~ IntegerClass, ClassificationOf (Value const b) ~ IntegerClass
     , ValueOf (Value const b)
     , BitsOf (Value const b) + 1 <= BitsOf (Value const a)
     , ValueJoin const)
  => Value const a
  -> BasicBlock (Value const b)
trunc = vmap1' f g where
  vt = valueType (Proxy :: Proxy (Value const b))
  f v = Constant.Trunc v vt
  g v = nameInstruction $ AST.Trunc v vt []

bitcast
  :: forall a b const .
     ( BitsOf (Value const a) ~ BitsOf (Value const b)
     , ValueOf (Value const b)
     , ValueJoin const)
  => Value const a
  -> BasicBlock (Value const b)
bitcast = vmap1' f g where
  vt = valueType (Proxy :: Proxy (Value const b))
  f v = Constant.BitCast v vt
  g v = nameInstruction $ AST.BitCast v vt []

class Add (classification :: Classification) where
  vadd
    :: ClassificationOf (Value (cx `Weakest` cy) a) ~ classification
    => Value cx a
    -> Value cy a
    -> Value (cx `Weakest` cy) a

instance Add 'IntegerClass where
 vadd = vmap2 f g where
   f = Constant.Add False False
   g x y = nameInstruction $ AST.Add False False x y []

instance Add 'FloatingPointClass where
 vadd = vmap2 f g where
   f = Constant.FAdd
   g x y = nameInstruction $ AST.FAdd x y []


add
  :: ( Add (ClassificationOf (Value (cx `Weakest` cy) a))
     , ValueJoin (cx `Weakest` cy))
  => Value cx a
  -> Value cy a
  -> BasicBlock (Value (cx `Weakest` cy) a)
add x y = vjoin $ vadd x y

-- the condition constness must match the result constness. this implies that
-- if both true and false values are constant the switch condition must also be
-- a constant. if you want a constant condition but mutable values (for some reason...)
-- just wrap the condition with 'mutable'
select
  :: (ValueJoin (cc `Weakest` ct `Weakest` cf))
  => Value cc Bool
  -> Value ct a
  -> Value cf a
  -> BasicBlock (Value (cc `Weakest` ct `Weakest` cf) a)
select = vmap3' f g where
  f = Constant.Select
  g c t f' = nameInstruction $ AST.Select c t f' []

icmp
  :: ( ClassificationOf (Value (cx `Weakest` cy) a) ~ IntegerClass
     , ValueJoin (cx `Weakest` cy))
  => IntegerPredicate.IntegerPredicate
  -> Value cx a
  -> Value cy a
  -> BasicBlock (Value (cx `Weakest` cy) Bool)
icmp p = vmap2' f g where
  f = Constant.ICmp p
  g x y = nameInstruction $ AST.ICmp p x y []

fcmp
  :: ( ClassificationOf (Value (cx `Weakest` cy) a) ~ FloatingPointClass
     , ValueJoin (cx `Weakest` cy))
  => FloatingPointPredicate.FloatingPointPredicate
  -> Value cx a
  -> Value cy a
  -> BasicBlock (Value (cx `Weakest` cy) Bool)
fcmp p = vmap2' f g where
  f = Constant.FCmp p
  g x y = nameInstruction $ AST.FCmp p x y []

class Cmp (classification :: Classification) where
  cmp
    :: ( ClassificationOf (Value (cx `Weakest` cy) a) ~ classification
       , ValueJoin (cx `Weakest` cy))
    => Value cx a
    -> Value cy a
    -> BasicBlock (Value (cx `Weakest` cy) Bool)

instance Cmp 'IntegerClass where
  cmp = vmap2' f g where
    f = Constant.ICmp IntegerPredicate.EQ
    g x y = nameInstruction $ AST.ICmp IntegerPredicate.EQ x y []

instance Cmp 'FloatingPointClass where
  cmp = vmap2' f g where
    f = Constant.FCmp FloatingPointPredicate.OEQ
    g x y = nameInstruction $ AST.FCmp FloatingPointPredicate.OEQ x y []
