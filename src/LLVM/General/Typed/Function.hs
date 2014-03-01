{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.General.Typed.Function where

import Foreign.Ptr (Ptr)
import GHC.TypeLits (KnownNat)
import qualified LLVM.General.AST.CallingConvention as CC

import LLVM.General.Typed.CallingConv
import LLVM.General.Typed.Value

-- |
-- 'Function's are 'Constant' 'Value's with a specific 'CallingConv'
data Function (cconv :: CallingConv) (a :: *) where
  Function :: KnownNat nat => Value 'Constant (Ptr a) -> CC.CallingConvention -> Function ('CallingConv nat) a
