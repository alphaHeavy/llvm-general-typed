{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module LLVM.General.Typed.DefineBasicBlock
  ( DefineBasicBlock
  , basicBlock
  , basicBlock_
  , namedBasicBlock
  , namedBasicBlock_
  ) where

import Control.Monad.RWS.Lazy
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.ArgumentList
import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.FunctionDefinition

basicBlock :: (DefineBasicBlock f rty, FreshName f, Monad f) => BasicBlock (Terminator rty a) -> f (a, Label rty)
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

class DefineBasicBlock f rty where
  namedBasicBlock :: AST.Name -> BasicBlock (Terminator rty a) -> f (a, Label rty)

instance DefineBasicBlock UntypedFunctionDefinition rty where
  namedBasicBlock n bb = do
    ~FunctionDefinitionState{functionDefinitionBasicBlocks = originalBlocks} <- get
    ~(newBlock, x) <- runBasicBlock n bb
    ~st@FunctionDefinitionState{functionDefinitionBasicBlocks = extraBlocks} <- get
    -- splice in the new block before any blocks defined while lifting
    let splicedBlocks = spliceBasicBlock originalBlocks newBlock extraBlocks
    put st{functionDefinitionBasicBlocks = splicedBlocks}
    return (x, Label n)

instance ReturnType ty ~ rty => DefineBasicBlock (FunctionDefinition ty) rty where
  namedBasicBlock n bb = FunctionDefinition (namedBasicBlock n bb)

instance DefineBasicBlock BasicBlock ty where
  namedBasicBlock n bb =
    liftFunctionDefinition (namedBasicBlock n bb)

basicBlock_ :: (DefineBasicBlock f rty, FreshName f, Monad f) => BasicBlock (Terminator rty ()) -> f (Label rty)
basicBlock_ = liftM snd . basicBlock

namedBasicBlock_ :: (DefineBasicBlock f rty, Monad f) => AST.Name -> BasicBlock (Terminator rty ()) -> f (Label rty)
namedBasicBlock_ n = liftM snd . namedBasicBlock n
