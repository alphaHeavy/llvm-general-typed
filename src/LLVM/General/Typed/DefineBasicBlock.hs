{-# LANGUAGE DataKinds #-}

module LLVM.General.Typed.DefineBasicBlock where

import Control.Monad.RWS.Lazy
import Data.List as List
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.FunctionDefinition

basicBlock :: (DefineBasicBlock f, FreshName f, Monad f) => BasicBlock (Terminator a) -> f (a, Label)
basicBlock bb = do
  n <- freshName
  namedBasicBlock n bb

class DefineBasicBlock f where
  namedBasicBlock :: AST.Name -> BasicBlock (Terminator a) -> f (a, Label)

instance DefineBasicBlock FunctionDefinition where
  namedBasicBlock n bb = do
    ~FunctionDefinitionState{functionDefinitionBasicBlocks = originalBlocks} <- get
    ~(x, newBlock) <- evalBasicBlock n bb
    ~st@FunctionDefinitionState{functionDefinitionBasicBlocks = extraBlocks} <- get
    -- splice in the new block before any blocks defined while lifting
    put st{functionDefinitionBasicBlocks = originalBlocks <> (newBlock:List.drop (List.length originalBlocks) extraBlocks)}
    return (x, Label n)

instance DefineBasicBlock BasicBlock where
  namedBasicBlock n bb =
    liftFunctionDefinition (namedBasicBlock n bb)

basicBlock_ :: (DefineBasicBlock f, FreshName f, Monad f) => BasicBlock (Terminator ()) -> f Label
basicBlock_ = liftM snd . basicBlock

namedBasicBlock_ :: (DefineBasicBlock f, Monad f) => AST.Name -> BasicBlock (Terminator ()) -> f Label
namedBasicBlock_ n = liftM snd . namedBasicBlock n
