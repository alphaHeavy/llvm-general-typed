{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.General.Typed.Function
  ( Function
  , createFunction
  , functionCallingConv
  , functionValue
  ) where

import Data.Proxy
import Foreign.Ptr (Ptr)
import GHC.TypeLits (KnownNat)
import qualified LLVM.General.AST.CallingConvention as CC

import LLVM.General.Typed.CallingConv
import LLVM.General.Typed.Value

-- |
-- 'Function's are wrappers around 'Constant' 'Value's tagged with a specific 'CallingConv'
--
-- @
-- 'Function' 'LLVM.General.Typed.CallingConv.C' ('Data.Int.Int64' 'Prelude.->' 'Data.Int.Int32') = 'Value' 'Constant' ('Ptr' ('Data.Int.Int64' 'Prelude.->' 'Data.Int.Int32'))
-- 'Function' 'LLVM.General.Typed.CallingConv.X86_StdCall' ('Foreign.C.CString.CString' -> 'Data.Int.Int32') = 'Value' 'Constant' ('Ptr' ('Foreign.C.CString.CString' -> 'Data.Int.Int32')) @
data Function (cconv :: CallingConv) (a :: *) where
  Function :: KnownNat nat => Value 'Constant (Ptr a) -> CC.CallingConvention -> Function ('CallingConv nat) a

createFunction
  :: forall a nat
   . KnownNat nat
  => Value 'Constant (Ptr a)
  -> Function ('CallingConv nat) a
createFunction value = Function value (reifyCallingConv (Proxy :: Proxy ('CallingConv nat)))

functionValue
  :: Function cconv a
  -> Value 'Constant (Ptr a)
functionValue (Function value _) = value

functionCallingConv
  :: Function cconv a
  -> CC.CallingConvention
functionCallingConv (Function _ cconv) = cconv
