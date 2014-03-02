{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.General.Typed.ValueWrap where

import Control.Monad
import Data.Typeable
import GHC.TypeLits
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as AST

import LLVM.General.Typed.CallingConv
import LLVM.General.Typed.Function
import LLVM.General.Typed.Value

operandWrap :: Typeable a => AST.Operand -> Maybe (Value const a)
operandWrap = undefined

functionWrap :: forall a cc . (KnownNat cc, Typeable a) => AST.Global -> Maybe (Function ('CallingConv cc) a)
functionWrap AST.Function{..} = do
  guard (callingConvention == reifyCallingConv (Proxy :: Proxy ('CallingConv cc)))
  undefined
