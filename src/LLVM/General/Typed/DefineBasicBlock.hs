{-# LANGUAGE DataKinds #-}

module LLVM.General.Typed.DefineBasicBlock where

import Control.Monad.RWS.Lazy
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.FunctionDefinition

basicBlock :: (DefineBasicBlock f, FreshName f, Monad f) => BasicBlock (Terminator a) -> f (a, Label)
basicBlock bb = do
  n <- freshName
  namedBasicBlock n bb

spliceBasicBlock
  :: [AST.BasicBlock] -- ^ the original block list
  -> AST.BasicBlock   -- ^ the block to splice
  -> [AST.BasicBlock] -- ^ the updated block list, must be >= to the original
  -> [AST.BasicBlock]
spliceBasicBlock [] newBlock extraBlocks = newBlock:extraBlocks
spliceBasicBlock (x:xs) newBlock (_:ys) = x:spliceBasicBlock xs newBlock ys
spliceBasicBlock _ _ _ = error "BasicBlocks should not be deleted"

class DefineBasicBlock f where
  namedBasicBlock :: AST.Name -> BasicBlock (Terminator a) -> f (a, Label)

instance DefineBasicBlock FunctionDefinition where
  namedBasicBlock n bb = do
    ~FunctionDefinitionState{functionDefinitionBasicBlocks = originalBlocks} <- get
    ~(newBlock, x) <- runBasicBlock n bb
    ~st@FunctionDefinitionState{functionDefinitionBasicBlocks = extraBlocks} <- get
    -- splice in the new block before any blocks defined while lifting
    let splicedBlocks = spliceBasicBlock originalBlocks newBlock extraBlocks
    put st{functionDefinitionBasicBlocks = splicedBlocks}
    return (x, Label n)

instance DefineBasicBlock BasicBlock where
  namedBasicBlock n bb =
    liftFunctionDefinition (namedBasicBlock n bb)

basicBlock_ :: (DefineBasicBlock f, FreshName f, Monad f) => BasicBlock (Terminator ()) -> f Label
basicBlock_ = liftM snd . basicBlock

namedBasicBlock_ :: (DefineBasicBlock f, Monad f) => AST.Name -> BasicBlock (Terminator ()) -> f Label
namedBasicBlock_ n = liftM snd . namedBasicBlock n
