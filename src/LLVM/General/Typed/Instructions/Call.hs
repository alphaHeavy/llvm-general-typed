{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module LLVM.General.Typed.Instructions.Call
  ( call
  ) where

import Control.Applicative
import Data.Proxy
import GHC.Generics
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Function
import LLVM.General.Typed.Instructions.Apply
import LLVM.General.Typed.Value

call
  :: forall args cconv ty
   . (Generic args, Apply (ArgumentList ty) (Rep args))
  => Function cconv ty
  -> args
  -> BasicBlock (Value 'Mutable (ApplicationResult (ArgumentList ty) (Rep args)))
call (Function (ValueConstant f) cconv) args = do
  let f' = AST.ConstantOperand f
  args' <- apply (Proxy :: Proxy (ArgumentList ty)) (from args)
  let instr = AST.Call
        { isTailCall = False
        , callingConvention = cconv
        , returnAttributes = []
        , function = Right f'
        , arguments = (,[]) <$> args'
        , functionAttributes = []
        , metadata = []
        }
  ValueOperand . return <$> nameInstruction instr
