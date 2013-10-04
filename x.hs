{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeHoles #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Control.Monad.State.Lazy
import Data.Int
import Data.List as List
import Data.Maybe (fromJust)
import Data.Traversable
import Data.Word
import Foreign.Ptr (Ptr)
import GHC.TypeLits
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant
import qualified LLVM.General.AST.Float as Float
import qualified LLVM.General.AST.Global as Global
import qualified LLVM.General.AST.FloatingPointPredicate as FloatingPointPredicate
import qualified LLVM.General.AST.IntegerPredicate as IntegerPredicate
import LLVM.General.PrettyPrint (showPretty)

data Constness = Constant | Mutable

type family Weakest (x :: k) (y :: k) :: k where
  Weakest 'Constant 'Constant = 'Constant
  Weakest x         y         = 'Mutable

data Value (const :: Constness) (a :: *) where
  ValueMutable     :: Value 'Constant a      -> Value 'Mutable a
  ValueOperand     :: BasicBlock AST.Operand -> Value 'Mutable a
  ValueConstant    :: Constant.Constant      -> Value 'Constant a

data AnyValue (a :: *) where
  AnyValue :: ValueOf (Value const a) => Value const a -> AnyValue a

mutable :: Value 'Constant a -> Value 'Mutable a
mutable = ValueMutable

constant :: Value 'Constant a -> Value 'Constant a
constant = id

class Weaken (const :: Constness) where
  weaken :: Value const a -> Value 'Mutable a

instance Weaken 'Constant where
  weaken = mutable

instance Weaken 'Mutable where
  weaken = id

class InjectConstant (const :: Constness) where
  injectConstant :: Constant.Constant -> Value const a

instance InjectConstant 'Mutable where
  injectConstant = ValueMutable . injectConstant

instance InjectConstant 'Constant where
  injectConstant = ValueConstant

data Classification
  = IntegerClass
  | FloatingPointClass
  | PointerClass
  | VectorClass
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

data Label = Label AST.Name

newtype Terminator a = Terminator a deriving (Functor, Show)

instance Applicative Terminator where
  pure = Terminator
  Terminator f <*> x = f <$> x

class FreshName (f :: * -> *) where
  freshName :: f AST.Name

instance FreshName BasicBlock where
  freshName =
    liftFunctionDefinition freshName

instance FreshName FunctionDefinition where
  freshName = do
    st@FunctionDefinitionState{functionDefinitionFreshId = fresh} <- get
    put $! st{functionDefinitionFreshId = fresh + 1}
    return $ AST.UnName fresh

pushNamedInstruction :: AST.Named AST.Instruction -> BasicBlock ()
pushNamedInstruction inst' = do
  tell [inst']

pushNamelessInstruction :: AST.Instruction -> BasicBlock ()
pushNamelessInstruction = pushNamedInstruction . AST.Do

nameAndPushInstruction :: AST.Instruction -> BasicBlock AST.Name
nameAndPushInstruction inst' = do
  n <- freshName
  pushNamedInstruction $ n AST.:= inst'
  return n

newtype Module a = Module{runModule :: State ModuleState a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState ModuleState)

data ModuleState = ModuleState
  { moduleName        :: String
  , moduleDefinitions :: [AST.Definition]
  }

newtype FunctionDefinition a = FunctionDefinition{runFunctionDefinition :: State FunctionDefinitionState a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState FunctionDefinitionState)

data FunctionDefinitionState = FunctionDefinitionState
  { functionDefinitionBasicBlocks :: [AST.BasicBlock]
  , functionDefinitionFreshId     :: {-# UNPACK #-} !Word
  }

newtype BasicBlock a = BasicBlock{runBasicBlock :: RWST () [AST.Named AST.Instruction] BasicBlockState FunctionDefinition a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState BasicBlockState, MonadWriter [AST.Named AST.Instruction])

liftFunctionDefinition :: FunctionDefinition a -> BasicBlock a
liftFunctionDefinition = BasicBlock . lift

data BasicBlockState = BasicBlockState
  { basicBlockName         :: AST.Name
  , basicBlockTerminator   :: Maybe (AST.Named AST.Terminator)
  } deriving (Show)

data CallingConvention where
  C :: CallingConvention
  Fast :: CallingConvention
  Cold :: CallingConvention
  GHC :: CallingConvention
  NumberedCC :: Nat -> CallingConvention

data Function (cconv :: CallingConvention) (a :: *)

newtype Globals a = Globals{runGlobals :: State [AST.Global] a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState [AST.Global])

evalModule :: Module a -> (AST.Module, a)
evalModule (Module a) = (m, a') where
  m = AST.Module n Nothing Nothing defs
  n = moduleName st'
  defs = moduleDefinitions st'
  st = ModuleState{moduleName = "unnamed module", moduleDefinitions = []}
  ~(a', st') = runState a st

evalBasicBlock :: AST.Name -> BasicBlock (Terminator a) -> FunctionDefinition (a, AST.BasicBlock)
evalBasicBlock n bb = do
  -- pattern match must be lazy to support the MonadFix instance
  ~(Terminator a, st, instr) <- runRWST (runBasicBlock bb) () (BasicBlockState n Nothing)
  return (a, AST.BasicBlock (basicBlockName st) instr (fromJust (basicBlockTerminator st)))

evalConstantBasicBlock :: BasicBlock (Value 'Constant a) -> Value 'Constant a
evalConstantBasicBlock (BasicBlock v) =
  let m = evalRWST v () (BasicBlockState (error "name") Nothing)
  in fst $ evalState (runFunctionDefinition m) (FunctionDefinitionState [] 0)

-- instance IsString (Value const String) where

nameAndEmitInstruction1
  :: (AST.Operand -> [t] -> AST.Instruction)
  -> Value const x
  -> Value 'Mutable a
nameAndEmitInstruction1 instr =
  apply $ \ x ->
    nameInstruction $ instr x []
 where
  apply
    :: (AST.Operand -> BasicBlock AST.Operand)
    -> Value const x
    -> Value 'Mutable a
  apply f (ValueOperand x)  = ValueOperand (x >>= f)
  apply f (ValueConstant x) = apply f (ValueOperand . return $ AST.ConstantOperand x)
  apply f (ValueMutable x)  = apply f x

nameAndEmitInstruction2
  :: (AST.Operand -> AST.Operand -> [t] -> AST.Instruction)
  -> Value cx x
  -> Value cy y
  -> Value 'Mutable a
nameAndEmitInstruction2 instr =
  apply2 $ \ x y ->
    nameInstruction $ instr x y []
 where
  apply2
    :: (AST.Operand -> AST.Operand -> BasicBlock AST.Operand)
    -> Value cx x
    -> Value cy y
    -> Value 'Mutable a
  apply2 f (ValueOperand x) (ValueOperand y) = ValueOperand . join $ f <$> x <*> y
  apply2 f (ValueConstant x) y = apply2 f (ValueOperand . return $ AST.ConstantOperand x) y
  apply2 f x (ValueConstant y) = apply2 f x (ValueOperand . return $ AST.ConstantOperand y)
  apply2 f (ValueMutable x) y  = apply2 f x y
  apply2 f x (ValueMutable y)  = apply2 f x y

applyConstant2 :: (Constant.Constant -> Constant.Constant -> Constant.Constant) -> Value 'Constant a -> Value 'Constant a -> Value 'Constant a
applyConstant2 instr (ValueConstant x) (ValueConstant y) =
  ValueConstant (instr x y)

signumSignedConst :: forall a . (SingI (BitsOf (Value 'Constant a))) => Value 'Constant a -> Value 'Constant a
signumSignedConst (ValueConstant x) = ValueConstant ig where
  bits = fromIntegral (fromSing (sing :: Sing (BitsOf (Value 'Constant a))))
  ig = Constant.Select gt (Constant.Int bits   1 ) il
  il = Constant.Select lt (Constant.Int bits (-1)) (Constant.Int bits 0)
  gt = Constant.ICmp IntegerPredicate.SGT x (Constant.Int bits 0)
  lt = Constant.ICmp IntegerPredicate.SLT x (Constant.Int bits 0)

signumUnsignedConst
  :: (SingI (BitsOf (Value 'Constant a)), ClassificationOf (Value 'Constant a) ~ IntegerClass, Num (Value 'Constant a))
  => Value 'Constant a
  -> Value 'Constant a
signumUnsignedConst x = evalConstantBasicBlock $ do
  gt <- icmp IntegerPredicate.UGT x (constant 0)
  select gt (constant 1) (constant 0)

fromIntegerConst :: forall a const . (SingI (BitsOf (Value const a)), InjectConstant const) => Integer -> Value const a
fromIntegerConst = injectConstant . Constant.Int bits where
  bits = fromIntegral $ fromSing (sing :: Sing (BitsOf (Value const a)))

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Float) where
  fromInteger = injectConstant . Constant.Float . Float.Single . fromIntegral
  (+) = vmap2 Constant.FAdd (nameInstruction2 AST.FAdd)
  (-) = vmap2 Constant.FSub (nameInstruction2 AST.FSub)
  (*) = vmap2 Constant.FMul (nameInstruction2 AST.FMul)

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Double) where
  fromInteger = injectConstant . Constant.Float . Float.Double . fromIntegral
  (+) = vmap2 Constant.FAdd (nameInstruction2 AST.FAdd)
  (-) = vmap2 Constant.FSub (nameInstruction2 AST.FSub)
  (*) = vmap2 Constant.FMul (nameInstruction2 AST.FMul)

instance (InjectConstant const, Weakest const const ~ const, Num (Value const Float)) => Fractional (Value const Float) where
  fromRational = injectConstant . Constant.Float . Float.Single . fromRational
  (/) = vmap2 Constant.FDiv (nameInstruction2 AST.FDiv)

instance (InjectConstant const, Weakest const const ~ const, Num (Value const Double)) => Fractional (Value const Double) where
  fromRational = injectConstant . Constant.Float . Float.Double . fromRational
  (/) = vmap2 Constant.FDiv (nameInstruction2 AST.FDiv)

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Int8) where
  fromInteger = fromIntegerConst
  -- abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  -- signum = signumUnsignedConst

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Int16) where
  fromInteger = fromIntegerConst
  -- abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  -- signum = signumUnsignedConst

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Int32) where
  fromInteger = fromIntegerConst
  -- abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  -- signum = signumUnsignedConst

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Int64) where
  fromInteger = fromIntegerConst
  -- abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  -- signum = signumUnsignedConst

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Word8) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  -- signum = signumUnsignedConst

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Word16) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  -- signum = signumUnsignedConst

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Word32) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  -- signum = signumUnsignedConst

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Word64) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  -- signum = signumUnsignedConst

namedModule :: String -> Globals a -> Module a
namedModule n body = do
  let ~(a, defs) = runState (runGlobals body) []
  st <- get
  put $!  st{moduleName = n, moduleDefinitions = fmap AST.GlobalDefinition defs}
  return a

namedFunction :: String -> FunctionDefinition a -> Globals (Function cconv ty, a)
namedFunction n defn = do
  let defnSt = FunctionDefinitionState{functionDefinitionBasicBlocks = [], functionDefinitionFreshId = 0}
      ~(a, defSt') = runState (runFunctionDefinition defn) defnSt
      x = AST.functionDefaults
           { Global.basicBlocks = functionDefinitionBasicBlocks defSt'
           , Global.name = AST.Name n
           , Global.returnType = AST.IntegerType 8
           }
  st <- get
  put $! x:st
  return (error "foo", a)

externalFunction :: String -> Globals ty
externalFunction = error "externalFunction"

basicBlock :: (DefineBasicBlock f, FreshName f, Monad f) => BasicBlock (Terminator ()) -> f Label
basicBlock bb = do
  n <- freshName
  namedBasicBlock n bb

class DefineBasicBlock f where
  namedBasicBlock :: AST.Name -> BasicBlock (Terminator ()) -> f Label

instance DefineBasicBlock FunctionDefinition where
  namedBasicBlock n bb = do
    ~FunctionDefinitionState{functionDefinitionBasicBlocks = originalBlocks} <- get
    (_, newBlock) <- evalBasicBlock n bb
    ~st@FunctionDefinitionState{functionDefinitionBasicBlocks = extraBlocks} <- get
    -- splice in the new block before any blocks defined while lifting
    put st{functionDefinitionBasicBlocks = originalBlocks <> (newBlock:List.drop (List.length originalBlocks) extraBlocks)}
    return $ Label n

instance DefineBasicBlock BasicBlock where
  namedBasicBlock n bb =
    liftFunctionDefinition (namedBasicBlock n bb)

asOp :: Value const a -> BasicBlock AST.Operand
asOp (ValueConstant x) = return $ AST.ConstantOperand x
asOp (ValueMutable x) = asOp x
asOp (ValueOperand x) = x

setTerminator :: AST.Terminator -> BasicBlock ()
setTerminator term = do
  st <- get
  put $! st{basicBlockTerminator = Just (AST.Do term)}

ret :: ValueOf (Value const a) => Value const a -> BasicBlock (Terminator ())
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

condBr :: Value const Bool -> Label -> Label -> BasicBlock (Terminator ())
condBr condition (Label trueDest) (Label falseDest) = do
  conditionOp <- asOp condition
  setTerminator $ AST.CondBr conditionOp trueDest falseDest []
  return $ Terminator ()

br :: Label -> BasicBlock (Terminator ())
br (Label dest) = do
  setTerminator $ AST.Br dest []
  return $ Terminator ()

switch
  :: (ClassificationOf (Value const a)     ~ IntegerClass,
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

unreachable :: BasicBlock (Terminator ())
unreachable = do
  setTerminator $ AST.Unreachable []
  return $ Terminator ()

undef :: forall a . ValueOf (Value 'Constant a) => BasicBlock (Value 'Constant a)
undef = do
  let val = Constant.Undef $ valueType ([] :: [Value 'Constant a])
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

    let ty = valueType ([] :: [Value 'Mutable a])
    n <- nameAndPushInstruction $ AST.Phi ty incomingValues' []
    return $! ValueOperand (return $ AST.LocalReference n)

instance Phi AnyValue where
  phi :: forall a . ValueOf (Value 'Mutable a) => [(AnyValue a, Label)] -> BasicBlock (Value 'Mutable a)
  phi incomingValues = do
    -- @TODO: make sure we have evaluated all of the values in the list...
    incomingValues' <- for incomingValues $ \ (AnyValue val, Label origin) -> do
      valOp <- asOp val
      return (valOp, origin)

    let ty = valueType ([] :: [Value 'Mutable a])
    n <- nameAndPushInstruction $ AST.Phi ty incomingValues' []
    return $! ValueOperand (return $ AST.LocalReference n)

alloca :: forall a . (ValueOf (Value 'Mutable a), SingI (ElementsOf (Value 'Mutable a))) => BasicBlock (Value 'Mutable (Ptr a))
alloca = do
  let ty = valueType ([] :: [Value 'Mutable a])
      ne = fromSing (sing :: Sing (ElementsOf (Value 'Mutable a)))
  -- @TODO: the hardcoded 64 should probably be the target word size?
  n <- nameAndPushInstruction $ AST.Alloca ty (Just (AST.ConstantOperand (Constant.Int 64 ne))) 0 []
  return $! ValueOperand (return $ AST.LocalReference n)

load :: Value const (Ptr a) -> BasicBlock (Value 'Mutable a)
load x = do
  x' <- asOp x
  n <- nameAndPushInstruction $ AST.Load False x' Nothing 0 []
  return $! ValueOperand (return (AST.LocalReference n))

store :: Value cx (Ptr a) -> Value cy a -> BasicBlock ()
store address value = do
  address' <- asOp address
  value' <- asOp value
  let instr = AST.Store False address' value' Nothing 0 []
  void . pushNamedInstruction $ AST.Do instr

type family ResultType a :: *

call :: Function cconv ty -> args -> BasicBlock (ResultType ty)
call = error "call"

nameInstruction :: AST.Instruction -> BasicBlock AST.Operand
nameInstruction instr = do
  n <- freshName
  tell [n AST.:= instr]
  return $ AST.LocalReference n

nameInstruction2
  :: (AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction)
  -> AST.Operand
  -> AST.Operand
  -> BasicBlock AST.Operand
nameInstruction2 f x y = nameInstruction (f x y [])

trunc
  :: forall a b const .
     (ClassificationOf (Value const a) ~ IntegerClass, ClassificationOf (Value const b) ~ IntegerClass
     ,ValueOf (Value const b)
     ,BitsOf (Value const b) + 1 <= BitsOf (Value const a)
     ,ValueJoin const)
  => Value const a
  -> BasicBlock (Value const b)
trunc = vmap1' f g where
  vt = valueType ([] :: [Value const b])
  f v = Constant.Trunc v vt
  g v = nameInstruction $ AST.Trunc v vt []

bitcast
  :: forall a b const . (BitsOf (Value const a) ~ BitsOf (Value const b), ValueOf (Value const b), ValueJoin const)
  => Value const a
  -> BasicBlock (Value const b)
bitcast = vmap1' f g where
  vt = valueType ([] :: [Value const b])
  f v = Constant.BitCast v vt
  g v = nameInstruction $ AST.BitCast v vt []

class ValueJoin (const :: Constness) where
  vjoin :: Value const a -> BasicBlock (Value const a)

instance ValueJoin 'Mutable where
  vjoin (ValueOperand a) = a >>= return . ValueOperand . return
  vjoin a = return a

instance ValueJoin 'Constant where
  vjoin a = return a

vmap1
  :: (Constant.Constant -> Constant.Constant)
  -> (AST.Operand -> BasicBlock AST.Operand)
  -> Value const a
  -> Value const b
vmap1 f _ (ValueConstant x) = ValueConstant (f x)
vmap1 f g (ValueMutable x)  = weaken (vmap1 f g x)
vmap1 _ g x@ValueOperand{}  = ValueOperand (join (g <$> asOp x))

vmap1'
  :: (ValueJoin const)
  => (Constant.Constant -> Constant.Constant)
  -> (AST.Operand -> BasicBlock AST.Operand)
  -> Value const a
  -> BasicBlock (Value const b)
vmap1' f g a = vjoin (vmap1 f g a)

vmap2
  :: forall a b cx cy r .
     (Constant.Constant -> Constant.Constant -> Constant.Constant)
  -> (AST.Operand -> AST.Operand -> BasicBlock AST.Operand)
  -> Value cx a
  -> Value cy b
  -> Value (Weakest cx cy) r
vmap2 f g = k where
  j :: Value cx a -> Value cy b -> Value 'Mutable r
  j x y = ValueOperand (join (g <$> asOp x <*> asOp y))
  k (ValueConstant x) (ValueConstant y) = ValueConstant (f x y)
  k (ValueMutable x)  (ValueMutable y)  = weaken (vmap2 f g x y)
  -- prepare to experience many pleasures of the GADT
  k x@ValueOperand{} y = j x y
  k x y@ValueOperand{} = j x y
  k x@ValueMutable{} y = j x y
  k x y@ValueMutable{} = j x y

vmap2'
  :: (ValueJoin (Weakest cx cy))
  => (Constant.Constant -> Constant.Constant -> Constant.Constant)
  -> (AST.Operand -> AST.Operand -> BasicBlock AST.Operand)
  -> Value cx a
  -> Value cy b
  -> BasicBlock (Value (Weakest cx cy) r)
vmap2' f g a b = vjoin (vmap2 f g a b)

vmap3
  :: forall a b c cx cy cz r .
     (Constant.Constant -> Constant.Constant -> Constant.Constant -> Constant.Constant)
  -> (AST.Operand -> AST.Operand -> AST.Operand -> BasicBlock AST.Operand)
  -> Value cx a
  -> Value cy b
  -> Value cz c
  -> Value (cx `Weakest` cy `Weakest` cz) r
vmap3 f g = k where
  j :: Value cx a -> Value cy b -> Value cz c -> Value 'Mutable r
  j x y z = ValueOperand (join (g <$> asOp x <*> asOp y <*> asOp z))
  k (ValueConstant x) (ValueConstant y) (ValueConstant z) = ValueConstant (f x y z)
  k (ValueMutable x)  (ValueMutable y)  (ValueMutable z)  = weaken (vmap3 f g x y z)
  -- prove we're dealing with a mutable result type
  k x@ValueOperand{} y z = j x y z
  k x y@ValueOperand{} z = j x y z
  k x y z@ValueOperand{} = j x y z
  k x@ValueMutable{} y z = j x y z
  k x y@ValueMutable{} z = j x y z
  k x y z@ValueMutable{} = j x y z

vmap3'
  :: (ValueJoin (cx `Weakest` cy `Weakest` cz))
  => (Constant.Constant -> Constant.Constant -> Constant.Constant -> Constant.Constant)
  -> (AST.Operand -> AST.Operand -> AST.Operand -> BasicBlock AST.Operand)
  -> Value cx a
  -> Value cy b
  -> Value cz c
  -> BasicBlock (Value (cx `Weakest` cy `Weakest` cz) r)
vmap3' f g a b c = vjoin (vmap3 f g a b c)

class Add (classification :: Classification) where
  add
    :: (ClassificationOf (Value (Weakest cx cy) a) ~ classification, ValueJoin (Weakest cx cy))
    => Value cx a
    -> Value cy a
    -> BasicBlock (Value (Weakest cx cy) a)

instance Add 'IntegerClass where
 add = vmap2' f g where
   f = Constant.Add False False
   g x y = nameInstruction $ AST.Add False False x y []

instance Add 'FloatingPointClass where
 add = vmap2' f g where
   f = Constant.FAdd
   g x y = nameInstruction $ AST.FAdd x y []

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
  :: (ClassificationOf (Value (Weakest cx cy) a) ~ IntegerClass, ValueJoin (Weakest cx cy))
  => IntegerPredicate.IntegerPredicate
  -> Value cx a
  -> Value cy a
  -> BasicBlock (Value (Weakest cx cy) Bool)
icmp p = vmap2' f g where
  f = Constant.ICmp p
  g x y = nameInstruction $ AST.ICmp p x y []

fcmp
  :: (ClassificationOf (Value (Weakest cx cy) a) ~ FloatingPointClass, ValueJoin (Weakest cx cy))
  => FloatingPointPredicate.FloatingPointPredicate
  -> Value cx a
  -> Value cy a
  -> BasicBlock (Value (Weakest cx cy) Bool)
fcmp p = vmap2' f g where
  f = Constant.FCmp p
  g x y = nameInstruction $ AST.FCmp p x y []

class Cmp (classification :: Classification) where
  cmp
    :: (ClassificationOf (Value (Weakest cx cy) a) ~ classification, ValueJoin (Weakest cx cy))
    => Value cx a
    -> Value cy a
    -> BasicBlock (Value (Weakest cx cy) Bool)

instance Cmp 'IntegerClass where
  cmp = vmap2' f g where
    f = Constant.ICmp IntegerPredicate.EQ
    g x y = nameInstruction $ AST.ICmp IntegerPredicate.EQ x y []

instance Cmp 'FloatingPointClass where
  cmp = vmap2' f g where
    f = Constant.FCmp FloatingPointPredicate.OEQ
    g x y = nameInstruction $ AST.FCmp FloatingPointPredicate.OEQ x y []

foo :: Module ()
foo = do
  let val :: Value 'Constant Word8
      val = 42 + 9

  namedModule "foo" $ do
    void . namedFunction "bar" $ mdo
      entryBlock <- basicBlock $ do
        br secondBlock

      secondBlock <- namedBasicBlock (AST.Name "second") $ do
        someLocalPtr <- alloca
        store someLocalPtr (99 :: Value 'Constant Word8)
        someLocal <- load someLocalPtr
        x <- val `add` someLocal
        join $ condBr
          <$> cmp someLocal (mutable 99)
          <*> basicBlock (ret $ x * someLocal + mutable (val - signum 8))
          <*> basicBlock (br entryBlock)

      return ()

main :: IO ()
main = do
  putStrLn . showPretty . fst $ evalModule foo
