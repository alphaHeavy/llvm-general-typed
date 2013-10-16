{-# LANGUAGE KindSignatures #-}

module FreshName where

import Control.Monad.RWS.Lazy
import qualified LLVM.General.AST as AST

import BasicBlock
import FunctionDefinition

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
  n <- freshName
  tell [n AST.:= instr]
  return $ AST.LocalReference n

nameInstruction2
  :: (AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction)
  -> AST.Operand
  -> AST.Operand
  -> BasicBlock AST.Operand
nameInstruction2 f x y = nameInstruction (f x y [])
