{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.Instructions.Call
  ( call
  , Apply(ApplicationResult)
  , ArgumentList
  ) where

import Control.Applicative
import Data.Proxy
import Data.Void
import GHC.Generics
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Function
import LLVM.General.Typed.Value

type family ArgumentList (args :: *) :: [*] where
  ArgumentList (a -> b) = a ': ArgumentList b
  ArgumentList a = '[a]

class Apply (args :: [*]) (f :: * -> *) where
  type ApplicationResult args f :: *
  apply :: proxy args -> f p -> BasicBlock [AST.Operand]

instance Apply xs f => Apply xs (M1 i c f) where
  type ApplicationResult xs (M1 i c f) = ApplicationResult xs f
  apply xs = apply xs . unM1

instance (Apply '[a] x, Apply as y) => Apply (a ': as) (x :*: y) where
  type ApplicationResult (a ': as) (x :*: y) = ApplicationResult as y -- The rightmost type is the result
  apply _ (x :*: y) = (++) <$> apply (Proxy :: Proxy '[a]) x <*> apply (Proxy :: Proxy as) y

instance x ~ a => Apply '[x] (K1 i (Value const a)) where
  type ApplicationResult '[x] (K1 i (Value const a)) = a
  apply _ = fmap (:[]) . asOp . unK1

instance f ~ Proxy "Extra arguments applied to function" => Apply '[] (K1 i f) where
  type ApplicationResult '[] (K1 i f) = Void
  apply _ _ = error "Extra arguments applied to function"

instance f ~ Proxy "Insufficient arguments applied to function" => Apply (a ': b ': c) (K1 i f) where
  type ApplicationResult (a ': b ': c) (K1 i f) = Void
  apply _ _ = error "Insufficient arguments applied to function"

call
  :: forall args cconv ty
   . (Generic args, Apply (ArgumentList ty) (Rep args))
  => Function cconv ty
  -> args
  -> BasicBlock (Value 'Mutable (ApplicationResult (ArgumentList ty) (Rep args)))
call (Function f cconv) args = do
  f' <- asOp f
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
