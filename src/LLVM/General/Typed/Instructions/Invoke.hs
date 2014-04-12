{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module LLVM.General.Typed.Instructions.Invoke
  ( invoke
  ) where

import Control.Applicative
import Data.Proxy
import GHC.Generics
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.Function
import LLVM.General.Typed.Instructions.Apply
import LLVM.General.Typed.Instructions.Call
import LLVM.General.Typed.Value

invoke
  :: forall args cconv ty
   . CanCall ty args
  => Function cconv ty
  -> args
  -> Label
  -> Label
  -> BasicBlock (Terminator (Value 'Mutable (CallResult ty args)))
invoke function args (Label returnDest) (Label exceptionDest) = do
  let ValueConstant f = functionValue function
      f' = AST.ConstantOperand f
      cconv = functionCallingConv function
  args' <- apply (Proxy :: Proxy (ArgumentList ty)) (from args)
  let instr = AST.Invoke
        { callingConvention' = cconv
        , returnAttributes' = []
        , function' = Right f'
        , arguments' = (,[]) <$> args'
        , functionAttributes' = []
        , returnDest = returnDest
        , exceptionDest = exceptionDest
        , metadata' = []
        }

  setTerminator instr
  error "Figure out how to return a Value from a Terminator"
