{-# LANGUAGE KindSignatures #-}

module LLVM.General.Typed.FreshName where

import Control.Monad.RWS.Lazy
import Data.Maybe (isJust)
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FunctionDefinition

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

nameInstruction :: AST.Instruction -> BasicBlock AST.Operand
nameInstruction instr = do
  BasicBlockState{basicBlockTerminator = term} <- get
  when (isJust term) $ fail "Terminator instruction has already been set for the current block"

  n <- freshName
  tell [n AST.:= instr]
  return $ AST.LocalReference n

nameInstruction2
  :: (AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction)
  -> AST.Operand
  -> AST.Operand
  -> BasicBlock AST.Operand
nameInstruction2 f x y = nameInstruction (f x y [])
