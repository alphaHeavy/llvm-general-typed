{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
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
import qualified LLVM.General.AST.IntegerPredicate as IntegerPredicate
import LLVM.General.PrettyPrint (showPretty)

data Constness = Constant | Mutable

type family (x :: k) :<+>: (y :: k) :: k

type instance 'Constant :<+>: 'Constant = 'Constant
type instance 'Constant :<+>: 'Mutable  = 'Mutable
type instance 'Mutable  :<+>: 'Constant = 'Mutable
type instance 'Mutable  :<+>: 'Mutable  = 'Mutable

newtype ValueContext a = ValueContext{runValueContext :: WriterT [AST.Named AST.Instruction] BasicBlock a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadWriter [AST.Named AST.Instruction])

data Value (const :: Constness) (a :: *) where
  ValueMutable     :: Value 'Constant a        -> Value 'Mutable a
  ValueOperand     :: ValueContext AST.Operand -> Value 'Mutable a
  ValueConstant    :: Constant.Constant        -> Value 'Constant a

data AnyValue (a :: *) where
  AnyValue :: ValueOf (Value const a) => Value const a -> AnyValue a

mutable :: Value 'Constant a -> Value 'Mutable a
mutable = ValueMutable

constant :: Value 'Constant a -> Value 'Constant a
constant = id

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

newtype Terminator a = Terminator a deriving (Show)

class FreshName (f :: * -> *) where
  freshName :: f AST.Name

instance FreshName ValueContext where
  freshName =
    liftBasicBlock freshName

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
  st@BasicBlockState{basicBlockInstructions = inst} <- get
  put $! st{basicBlockInstructions = inst <> [inst']}

pushNamelessInstruction :: AST.Instruction -> BasicBlock ()
pushNamelessInstruction = pushNamedInstruction . AST.Do

nameAndPushInstruction :: AST.Instruction -> BasicBlock AST.Name
nameAndPushInstruction inst' = do
  name <- freshName
  pushNamedInstruction $ name AST.:= inst'
  return name

apply
  -- :: (cx :<+>: cx) ~ 'Mutable
  :: (AST.Operand -> ValueContext AST.Operand)
  -> Value const x
  -> Value 'Mutable a
apply f (ValueOperand x)  = ValueOperand (x >>= f)
apply f (ValueConstant x) = apply f (ValueOperand . return $ AST.ConstantOperand x)
apply f (ValueMutable x)  = apply f x

apply2
  -- :: (cx :<+>: cx) ~ 'Mutable
  :: (AST.Operand -> AST.Operand -> ValueContext AST.Operand)
  -> Value cx x
  -> Value cy y
  -> Value 'Mutable a
apply2 f (ValueOperand x) (ValueOperand y) = ValueOperand $ x >>= \ op1 -> y >>= \ op2 -> f op1 op2
apply2 f (ValueConstant x) y = apply2 f (ValueOperand . return $ AST.ConstantOperand x) y
apply2 f x (ValueConstant y) = apply2 f x (ValueOperand . return $ AST.ConstantOperand y)
apply2 f (ValueMutable x) y  = apply2 f x y
apply2 f x (ValueMutable y)  = apply2 f x y

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

newtype BasicBlock a = BasicBlock{runBasicBlock :: StateT BasicBlockState FunctionDefinition a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState BasicBlockState)

liftBasicBlock :: BasicBlock a -> ValueContext a
liftBasicBlock = ValueContext . lift

liftFunctionDefinition :: FunctionDefinition a -> BasicBlock a
liftFunctionDefinition = BasicBlock . lift

data BasicBlockState = BasicBlockState
  { basicBlockName         :: AST.Name
  , basicBlockInstructions :: [AST.Named AST.Instruction]
  , basicBlockTerminator   :: Maybe (AST.Named AST.Terminator)
  } deriving (Show)

data Function a

newtype Globals a = Globals{runGlobals :: State [AST.Global] a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState [AST.Global])

evalModule :: Module a -> (AST.Module, a)
evalModule (Module a) = (m, a') where
  m = AST.Module name Nothing Nothing defs
  name = moduleName st'
  defs = moduleDefinitions st'
  st = ModuleState{moduleName = "unnamed module", moduleDefinitions = []}
  ~(a', st') = runState a st

evalBasicBlock :: AST.Name -> BasicBlock (Terminator a) -> FunctionDefinition (a, AST.BasicBlock)
evalBasicBlock name bb = do
  -- pattern match must be lazy to support the MonadFix instance
  ~(Terminator a, st) <- runStateT (runBasicBlock bb) (BasicBlockState name [] Nothing)
  return (a, AST.BasicBlock (basicBlockName st) (basicBlockInstructions st) (fromJust (basicBlockTerminator st)))

-- instance IsString (Value const String) where

nameAndEmitInstruction2
  :: (AST.Operand -> AST.Operand -> [t] -> AST.Instruction)
  -> Value cx x
  -> Value cy y
  -> Value 'Mutable a
nameAndEmitInstruction2 instr =
  apply2 $ \ x y -> do
    name <- freshName
    tell [name AST.:= instr x y []]
    return $ AST.LocalReference name

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

signumUnsignedConst :: forall a . (SingI (BitsOf (Value 'Constant a))) => Value 'Constant a -> Value 'Constant a
signumUnsignedConst (ValueConstant x) = ValueConstant ig where
  bits = fromIntegral (fromSing (sing :: Sing (BitsOf (Value 'Constant a))))
  ig = Constant.Select gt (Constant.Int bits 1) (Constant.Int bits 0)
  gt = Constant.ICmp IntegerPredicate.UGT x (Constant.Int bits 0)

fromIntegerConst :: forall a . (SingI (BitsOf (Value 'Constant a))) => Integer -> Value 'Constant a
fromIntegerConst = ValueConstant . Constant.Int bits where
 bits = fromIntegral $ fromSing (sing :: Sing (BitsOf (Value 'Constant a)))

instance Num (Value 'Constant Int8) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = applyConstant2 (Constant.Add False False)
  (-) = applyConstant2 (Constant.Sub False False)
  (*) = applyConstant2 (Constant.Mul False False)
  signum = signumSignedConst

instance Num (Value 'Constant Int16) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = applyConstant2 (Constant.Add False False)
  (-) = applyConstant2 (Constant.Sub False False)
  (*) = applyConstant2 (Constant.Mul False False)
  signum = signumSignedConst

instance Num (Value 'Constant Int32) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = applyConstant2 (Constant.Add False False)
  (-) = applyConstant2 (Constant.Sub False False)
  (*) = applyConstant2 (Constant.Mul False False)
  signum = signumSignedConst

instance Num (Value 'Constant Int64) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = applyConstant2 (Constant.Add False False)
  (-) = applyConstant2 (Constant.Sub False False)
  (*) = applyConstant2 (Constant.Mul False False)
  signum = signumSignedConst

instance Num (Value 'Constant Word8) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = applyConstant2 (Constant.Add False False)
  (-) = applyConstant2 (Constant.Sub False False)
  (*) = applyConstant2 (Constant.Mul False False)
  signum = signumUnsignedConst

instance Num (Value 'Constant Word16) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = applyConstant2 (Constant.Add False False)
  (-) = applyConstant2 (Constant.Sub False False)
  (*) = applyConstant2 (Constant.Mul False False)
  signum = signumUnsignedConst

instance Num (Value 'Constant Word32) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = applyConstant2 (Constant.Add False False)
  (-) = applyConstant2 (Constant.Sub False False)
  (*) = applyConstant2 (Constant.Mul False False)
  signum = signumUnsignedConst

instance Num (Value 'Constant Word64) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = applyConstant2 (Constant.Add False False)
  (-) = applyConstant2 (Constant.Sub False False)
  (*) = applyConstant2 (Constant.Mul False False)
  signum = signumUnsignedConst

instance Num (Value 'Constant Float) where
  fromInteger = ValueConstant . Constant.Float . Float.Single . fromIntegral
  abs = id
  (+) = applyConstant2 Constant.FAdd
  (-) = applyConstant2 Constant.FSub
  (*) = applyConstant2 Constant.FMul
  -- signum = signumUnsignedConst

instance Num (Value 'Constant Double) where
  fromInteger = ValueConstant . Constant.Float . Float.Double . fromIntegral
  abs = id
  (+) = applyConstant2 Constant.FAdd
  (-) = applyConstant2 Constant.FSub
  (*) = applyConstant2 Constant.FMul

instance Fractional (Value 'Constant Float) where
  fromRational = ValueConstant . Constant.Float . Float.Single . fromRational
  (/) = applyConstant2 Constant.FDiv

instance Fractional (Value 'Constant Double) where
  fromRational = ValueConstant . Constant.Float . Float.Double . fromRational
  (/) = applyConstant2 Constant.FDiv

{-
instance Num (Value 'Mutable Int8) where
  fromInteger = ValueMutable . fromInteger

instance Num (Value 'Mutable Int16) where
  fromInteger = ValueMutable . fromInteger

instance Num (Value 'Mutable Int32) where
  fromInteger = ValueMutable . fromInteger

instance Num (Value 'Mutable Int64) where
  fromInteger = ValueMutable . fromInteger
-}

instance Num (Value 'Mutable Word8) where
  fromInteger = ValueMutable . fromInteger
  abs = id
  (+) = nameAndEmitInstruction2 (AST.Add False False)
  (-) = nameAndEmitInstruction2 (AST.Sub False False)
  (*) = nameAndEmitInstruction2 (AST.Mul False False)

{-
instance Num (Value 'Mutable Word16) where
  fromInteger = ValueMutable . fromInteger
  abs = id
  -- ValueConstant x + ValueConstant y = ValueConstant (Constant.Add False False x y)

instance Num (Value 'Mutable Word32) where
  fromInteger = ValueMutable . fromInteger
  abs = id
  -- ValueConstant x + ValueConstant y = ValueConstant (Constant.Add False False x y)

instance Num (Value 'Mutable Word64) where
  fromInteger = ValueMutable . fromInteger
  abs = id
  (+) = apply $ \ x y -> ValueOperand $ do
          name <- get
          put $! name + 1
          tell [LLVM.Add False False x y []]
          return $ LLVM.LocalReference (LLVM.UnName name)
-}

instance Num (Value 'Mutable Float) where
  fromInteger = ValueMutable . fromInteger
  abs = id
  (+) = nameAndEmitInstruction2 AST.FAdd
  (-) = nameAndEmitInstruction2 AST.FSub
  (*) = nameAndEmitInstruction2 AST.FMul

instance Num (Value 'Mutable Double) where
  fromInteger = ValueMutable . fromInteger
  abs = id
  (+) = nameAndEmitInstruction2 AST.FAdd
  (-) = nameAndEmitInstruction2 AST.FSub
  (*) = nameAndEmitInstruction2 AST.FMul

instance Fractional (Value 'Mutable Float) where
  fromRational = ValueMutable . fromRational
  (/) = nameAndEmitInstruction2 AST.FDiv

instance Fractional (Value 'Mutable Double) where
  fromRational = ValueMutable . fromRational
  (/) = nameAndEmitInstruction2 AST.FDiv

namedModule :: String -> Globals a -> Module a
namedModule name body = do
  let ~(a, defs) = runState (runGlobals body) []
  st <- get
  put $!  st{moduleName = name, moduleDefinitions = fmap AST.GlobalDefinition defs}
  return a

namedFunction :: String -> FunctionDefinition a -> Globals (Function ty, a)
namedFunction name defn = do
  let defnSt = FunctionDefinitionState{functionDefinitionBasicBlocks = [], functionDefinitionFreshId = 0}
      ~(a, defSt') = runState (runFunctionDefinition defn) defnSt
      x = AST.functionDefaults
           { Global.basicBlocks = functionDefinitionBasicBlocks defSt'
           , Global.name = AST.Name "give me a name"
           , Global.returnType = AST.IntegerType 8
           }
  st <- get
  put $! x:st
  return (error "foo", a)

externalFunction :: String -> Globals ty
externalFunction = error "externalFunction"

class DefineBasicBlock f where
  basicBlock :: BasicBlock (Terminator ()) -> f Label
  default basicBlock :: (FreshName f, Monad f) => BasicBlock (Terminator ()) -> f Label
  basicBlock bb = do
    name <- freshName
    namedBasicBlock name bb

  namedBasicBlock :: AST.Name -> BasicBlock (Terminator ()) -> f Label

instance DefineBasicBlock FunctionDefinition where
  namedBasicBlock name bb = do
    ~FunctionDefinitionState{functionDefinitionBasicBlocks = originalBlocks} <- get
    (_, newBlock) <- evalBasicBlock name bb
    ~st@FunctionDefinitionState{functionDefinitionBasicBlocks = extraBlocks} <- get
    -- splice in the new block before any blocks defined while lifting
    put st{functionDefinitionBasicBlocks = originalBlocks <> (newBlock:List.drop (List.length originalBlocks) extraBlocks)}
    return (Label name)

instance DefineBasicBlock BasicBlock where
  namedBasicBlock name bb =
    liftFunctionDefinition (namedBasicBlock name bb)

asOp :: Value const a -> BasicBlock AST.Operand
asOp (ValueConstant x) = return $ AST.ConstantOperand x
asOp (ValueMutable x) = asOp x
asOp (ValueOperand x) = do
  st@BasicBlockState{basicBlockInstructions = inst} <- get
  ~(x', inst') <- runWriterT $ runValueContext x
  put $! st{basicBlockInstructions = inst <> inst'}
  return x'

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
    name <- nameAndPushInstruction $ AST.Phi ty incomingValues' []
    return $! ValueOperand (return $ AST.LocalReference name)

instance Phi AnyValue where
  phi :: forall a . ValueOf (Value 'Mutable a) => [(AnyValue a, Label)] -> BasicBlock (Value 'Mutable a)
  phi incomingValues = do
    -- @TODO: make sure we have evaluated all of the values in the list...
    incomingValues' <- for incomingValues $ \ (AnyValue val, Label origin) -> do
      valOp <- asOp val
      return (valOp, origin)

    let ty = valueType ([] :: [Value 'Mutable a])
    name <- nameAndPushInstruction $ AST.Phi ty incomingValues' []
    return $! ValueOperand (return $ AST.LocalReference name)

alloca :: forall a . (ValueOf (Value 'Mutable a), SingI (ElementsOf (Value 'Mutable a))) => BasicBlock (Value 'Mutable (Ptr a))
alloca = do
  let ty = valueType ([] :: [Value 'Mutable a])
      ne = fromSing (sing :: Sing (ElementsOf (Value 'Mutable a)))
  name <- nameAndPushInstruction $ AST.Alloca ty (Just (AST.ConstantOperand (Constant.Int 64 ne))) 0 []
  return $! ValueOperand (return $ AST.LocalReference name)

load :: Value const (Ptr a) -> BasicBlock (Value 'Mutable a)
load x = do
  x' <- asOp x
  name <- nameAndPushInstruction $ AST.Load False x' Nothing 0 []
  return $! ValueOperand (return (AST.LocalReference name))

store :: Value cx (Ptr a) -> Value cy a -> BasicBlock ()
store address value = do
  address' <- asOp address
  value' <- asOp value
  let instr = AST.Store False address' value' Nothing 0 []
  void . pushNamedInstruction $ AST.Do instr

type family ResultType a :: *

call :: Function ty -> args -> BasicBlock (ResultType ty)
call = error "call"

class Add (const :: Constness) where
  add :: (cx :<+>: cy) ~ const => Value cx a -> Value cy a -> BasicBlock (Value const a)

instance Add 'Mutable where
  add x y = do
    res <- asOp $ nameAndEmitInstruction2 (AST.Add False False) x y
    return . ValueOperand $ pure res

instance Add 'Constant where
  ValueConstant x `add` ValueConstant y =
    return . ValueConstant $ Constant.Add False False x y

class Select (const :: Constness) where
  -- the condition constness must match the result constness. this implies that
  -- if both true and false values are constant the switch condition must also be
  -- a constant. if you want a constant condition but mutable values (for some reason...)
  -- just wrap the condition with 'mutable'
  select
    :: (cx :<+>: cy) ~ const
    => Value const Bool
    -> Value cx a
    -> Value cy a
    -> BasicBlock (Value const a)

instance Select 'Mutable where
  select condition trueValue falseValue = do
    conditionOp <- asOp condition
    trueValueOp <- asOp trueValue
    falseValueOp <- asOp falseValue
    let instr = AST.Select conditionOp trueValueOp falseValueOp []
    name <- nameAndPushInstruction instr
    return $! ValueOperand (return $ AST.LocalReference name)

instance Select 'Constant where
  select (ValueConstant condition) (ValueConstant trueValue) (ValueConstant falseValue) = do
    let instr = Constant.Select condition trueValue falseValue
    return $ ValueConstant instr

cmp :: Value cx a -> Value cy a -> BasicBlock (Value 'Mutable Bool)
cmp (ValueMutable x) y = cmp x y
cmp x (ValueMutable y) = cmp x y
cmp lhs rhs = do
  return (ValueMutable (ValueConstant (Constant.Int 1 1)))

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
