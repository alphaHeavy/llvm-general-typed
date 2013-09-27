{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeHoles #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Control.Monad.State.Lazy
import Data.Int
import Data.Maybe (fromJust)
import Data.String
import Data.Void
import Data.Word
import Foreign.Ptr (Ptr)
import GHC.TypeLits
import qualified LLVM.General as LLVM
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant
import LLVM.General.PrettyPrint (showPretty)

data Constness = Constant | Mutable

type family (x :: k) :<+>: (y :: k) :: k

type instance 'Constant :<+>: 'Constant = 'Constant
type instance 'Constant :<+>: 'Mutable  = 'Mutable
type instance 'Mutable  :<+>: 'Constant = 'Mutable
type instance 'Mutable  :<+>: 'Mutable  = 'Mutable

-- type family ConstSelector (a :: Const) (b :: Const) :: Const where
  -- ConstSelector 'Const 'Const = 'Const
  -- ConstSelector a      b      = 'Mutable
  --
type ValueContext a = RWS () [AST.Named AST.Instruction] Word a

data Value (const :: Constness) (a :: *) where
  ValueMutable     :: Value 'Constant a        -> Value 'Mutable a
  ValueOperand     :: ValueContext AST.Operand -> Value 'Mutable a
  ValueConstant    :: Constant.Constant        -> Value 'Constant a

mutable :: Value 'Constant a -> Value 'Mutable a
mutable = ValueMutable

instance Show (Value const a) where
  show (ValueMutable  c) = show c
  show (ValueConstant c) = show c
  show (ValueOperand  c) = show (evalRWS c () 0)

data Classification = IntegerClass | FloatingPointClass | PointerClass | VectorClass | StructureClass | LabelClass | MetadataClass

class ValueOf (a :: *) where
  type SizeOf a :: Nat
  type ElementsOf a :: Nat
  type ClassificationOf a :: Classification
  valueType :: proxy a -> AST.Type

instance ValueOf (Value 'Constant Word16) where
  type SizeOf (Value 'Constant Word16) = 2
  type ElementsOf (Value 'Constant Word16) = 1
  type ClassificationOf (Value 'Constant Word16) = IntegerClass

instance ValueOf (Value 'Constant Word8) where
  type SizeOf (Value 'Constant Word8) = 1
  type ElementsOf (Value 'Constant Word8) = 1
  type ClassificationOf (Value 'Constant Word8) = IntegerClass

instance ValueOf (Value 'Mutable Word8) where
  type SizeOf (Value 'Mutable Word8) = 1
  type ElementsOf (Value 'Mutable Word8) = 1
  type ClassificationOf (Value 'Mutable Word8) = IntegerClass
  valueType _ = AST.IntegerType 8

data Label = Label AST.Name

newtype Terminator a = Terminator a deriving (Show)

freshName :: BasicBlock AST.Name
freshName = do
  st@BasicBlockState{basicBlockFreshId = fresh} <- get
  put $! st{basicBlockFreshId = fresh + 1}
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
  :: (AST.Operand -> AST.Operand -> ValueContext AST.Operand)
  -> Value cx x
  -> Value cy y
  -> Value 'Mutable a
apply f (ValueOperand x) (ValueOperand y) = ValueOperand $ x >>= \ op1 -> y >>= \ op2 -> f op1 op2
apply f (ValueConstant x) y = apply f (ValueOperand . return $ AST.ConstantOperand x) y
apply f x (ValueConstant y) = apply f x (ValueOperand . return $ AST.ConstantOperand y)
apply f (ValueMutable x) y = apply f x y
apply f x (ValueMutable y) = apply f x y

newtype Module a = Module{runModule :: State ModuleState a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState ModuleState)

data ModuleState = ModuleState
  { moduleName        :: String
  , moduleDefinitions :: [AST.Definition]
  }

newtype FunctionDefinition ty a = FunctionDefinition{runFunctionDefinition :: State FunctionDefinitionState a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState FunctionDefinitionState)

data FunctionDefinitionState = FunctionDefinitionState
  { functionDefinitionBasicBlocks ::  [AST.BasicBlock]
  }

newtype BasicBlock a = BasicBlock{runBasicBlock :: State BasicBlockState a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState BasicBlockState)

data BasicBlockState = BasicBlockState
  { basicBlockName         :: AST.Name
  , basicBlockInstructions :: [AST.Named AST.Instruction]
  , basicBlockTerminator   :: Maybe (AST.Named AST.Terminator)
  , basicBlockFreshId      :: {-# UNPACK #-} !Word
  } deriving (Show)

data Function a
data Globals a

instance IsString (Value const String) where

nameAndEmitInstruction instr =
  apply $ \ x y -> do
    val <- get
    put $! val + 1
    let name = AST.UnName val
    tell [name AST.:= instr x y []]
    return $ AST.LocalReference name

applyConstant :: (Constant.Constant -> Constant.Constant -> Constant.Constant) -> Value 'Constant a -> Value 'Constant a -> Value 'Constant a
applyConstant instr (ValueConstant x) (ValueConstant y) =
  ValueConstant (instr x y)

instance Num (Value 'Constant Int8) where
  fromInteger = ValueConstant . Constant.Int 8
  abs = id
  (+) = applyConstant (Constant.Add False False)
  (-) = applyConstant (Constant.Sub False False)
  (*) = applyConstant (Constant.Mul False False)

instance Num (Value 'Constant Int16) where
  fromInteger = ValueConstant . Constant.Int 16
  abs = id
  (+) = applyConstant (Constant.Add False False)
  (-) = applyConstant (Constant.Sub False False)
  (*) = applyConstant (Constant.Mul False False)

instance Num (Value 'Constant Int32) where
  fromInteger = ValueConstant . Constant.Int 32
  abs = id
  (+) = applyConstant (Constant.Add False False)
  (-) = applyConstant (Constant.Sub False False)
  (*) = applyConstant (Constant.Mul False False)

instance Num (Value 'Constant Int64) where
  fromInteger = ValueConstant . Constant.Int 64
  abs = id
  (+) = applyConstant (Constant.Add False False)
  (-) = applyConstant (Constant.Sub False False)
  (*) = applyConstant (Constant.Mul False False)

instance Num (Value 'Constant Word8) where
  fromInteger = ValueConstant . Constant.Int 8
  abs = id
  (+) = applyConstant (Constant.Add False False)
  (-) = applyConstant (Constant.Sub False False)
  (*) = applyConstant (Constant.Mul False False)

instance Num (Value 'Constant Word16) where
  fromInteger = ValueConstant . Constant.Int 16
  abs = id
  (+) = applyConstant (Constant.Add False False)
  (-) = applyConstant (Constant.Sub False False)
  (*) = applyConstant (Constant.Mul False False)

instance Num (Value 'Constant Word32) where
  fromInteger = ValueConstant . Constant.Int 32
  abs = id
  (+) = applyConstant (Constant.Add False False)
  (-) = applyConstant (Constant.Sub False False)
  (*) = applyConstant (Constant.Mul False False)

instance Num (Value 'Constant Word64) where
  fromInteger = ValueConstant . Constant.Int 64
  abs = id
  (+) = applyConstant (Constant.Add False False)
  (-) = applyConstant (Constant.Sub False False)
  (*) = applyConstant (Constant.Mul False False)

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
  (+) = nameAndEmitInstruction (AST.Add False False)
  (-) = nameAndEmitInstruction (AST.Sub False False)
  (*) = nameAndEmitInstruction (AST.Mul False False)

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

namedModule :: String -> Globals a -> Module a
namedModule name body = do
  undefined

namedFunction :: String -> FunctionDefinition ty a -> Globals (Function ty, a)
namedFunction = undefined

externalFunction :: String -> Globals ty
externalFunction = undefined

basicBlock :: BasicBlock (Terminator a) -> FunctionDefinition ty (Label, a)
basicBlock = undefined

namedBasicBlock :: String -> BasicBlock (Terminator a) -> FunctionDefinition ty (Label, a)
namedBasicBlock = undefined

asOp :: Value const a -> BasicBlock AST.Operand
asOp (ValueOperand x) = do
  st@BasicBlockState{basicBlockFreshId = fresh, basicBlockInstructions = inst} <- get
  let (x', fresh', inst') = runRWS x () fresh
  put $! st{basicBlockFreshId = fresh', basicBlockInstructions = inst <> inst'}
  return x'

asOp (ValueConstant x) =
  return $ AST.ConstantOperand x

asOp (ValueMutable x) = asOp x

ret :: ValueOf (Value const a) => Value const a -> BasicBlock (Terminator (Value const a))
ret x = do
  -- name the value, emitting instructions as necessary
  x' <- asOp x
  st <- get
  put $! st{basicBlockTerminator = Just (AST.Do (AST.Ret (Just x') []))}
  return $ Terminator x -- (ValueOperand (return x'))

ret_ :: BasicBlock (Terminator ())
ret_ = undefined

condBr :: Value const Bool -> Label -> Label -> BasicBlock (Terminator ())
condBr = undefined

br :: Label -> BasicBlock (Terminator ())
br = undefined

switch = undefined

indirectBr = undefined

invoke = undefined

resume = undefined

unreachable :: BasicBlock (Terminator ())
unreachable = undefined

undef :: ValueOf (Value const a) => BasicBlock (Value const a)
undef = undefined

phi :: ValueOf (Value const a) => [(Value const a, Label)] -> BasicBlock (Value const a)
phi = undefined

alloca :: forall const a . (ValueOf (Value 'Mutable a), SingI (ElementsOf (Value 'Mutable a))) => BasicBlock (Value 'Mutable (Ptr a))
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
  void . pushNamedInstruction . AST.Do $ AST.Store False address' value' Nothing 0 []

type family ResultType a :: *

call :: Function ty -> args -> BasicBlock (ResultType ty)
call = undefined

foo =
  namedModule "foo" $ do
    namedFunction "bar" $ do
      rec (entryBlock, _) <- basicBlock $ do
            br secondBlock

          (secondBlock, eight) <- namedBasicBlock "second" $ do
            ret $ 4 + (4 :: Value 'Constant Word16)

      -- eight

      return ()

evalModule :: Module a -> (AST.Module, a)
evalModule (Module a) = undefined

evalBasicBlock :: BasicBlock (Terminator a) -> AST.BasicBlock
evalBasicBlock bb =
  let (_, st) = runState (runBasicBlock bb) (BasicBlockState (AST.Name "name") [] Nothing 0)
  -- print $ ("val", a)
  in AST.BasicBlock (basicBlockName st) (basicBlockInstructions st) (fromJust (basicBlockTerminator st))

main :: IO ()
main = do
  let val :: Value 'Mutable Word8
      val = 42 + 9

  putStrLn . showPretty . evalBasicBlock $ do
    someLocalPtr <- alloca
    store someLocalPtr (99 :: Value 'Constant Word8)
    someLocal <- load someLocalPtr
    ret $ someLocal + val
