{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}

module DefineBasicBlock where

import Control.Monad.RWS.Lazy
import Data.List as List
import qualified LLVM.General.AST as AST

import BasicBlock
import FreshName
import FunctionDefinition

basicBlock :: (DefineBasicBlock f, FreshName f, Monad f) => BasicBlock (Terminator ()) -> f Label
basicBlock bb = do
  n <- freshName
  namedBasicBlock n bb

class DefineBasicBlock f where
  namedBasicBlock :: AST.Name -> BasicBlock (Terminator ()) -> f Label

instance DefineBasicBlock FunctionDefinition where
  namedBasicBlock n bb = do
    ~FunctionDefinitionState{functionDefinitionBasicBlocks = originalBlocks} <- get
    ~(_, newBlock) <- evalBasicBlock n bb
    ~st@FunctionDefinitionState{functionDefinitionBasicBlocks = extraBlocks} <- get
    -- splice in the new block before any blocks defined while lifting
    put st{functionDefinitionBasicBlocks = originalBlocks <> (newBlock:List.drop (List.length originalBlocks) extraBlocks)}
    return $ Label n

instance DefineBasicBlock BasicBlock where
  namedBasicBlock n bb =
    liftFunctionDefinition (namedBasicBlock n bb)
