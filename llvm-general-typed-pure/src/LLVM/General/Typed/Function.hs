{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LLVM.General.Typed.Function
  ( Function(..)
  , createFunction
  , functionCallingConv
  , functionValue
  , getParameter
  , tryGetParameter
  ) where

import Control.Monad (guard)
import Control.Monad.State.Lazy (gets)
import Data.Proxy
import Foreign.Ptr (Ptr)
import GHC.TypeLits
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.CallingConvention as CC

import LLVM.General.Typed.ArgumentList
import LLVM.General.Typed.CallingConv
import LLVM.General.Typed.FunctionDefinition
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf

-- |
-- 'Function's are wrappers around 'Constant' 'Value's tagged with a specific 'CallingConv'
--
-- @
-- 'Function' 'LLVM.General.Typed.CallingConv.C' ('Data.Int.Int64' 'Prelude.->' 'Data.Int.Int32') = 'Value' 'Constant' ('Ptr' ('Data.Int.Int64' 'Prelude.->' 'Data.Int.Int32'))
-- 'Function' 'LLVM.General.Typed.CallingConv.X86_StdCall' ('Foreign.C.CString.CString' -> 'Data.Int.Int32') = 'Value' 'Constant' ('Ptr' ('Foreign.C.CString.CString' -> 'Data.Int.Int32')) @
data Function (cconv :: CallingConv) (a :: *) where
  Function :: Value 'Constant (Ptr a) -> CC.CallingConvention -> Function ('CallingConv nat) a

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

getParameter :: (ParameterType ty n ~ a, KnownNat n) => proxy n -> FunctionDefinition ty (Value 'Operand a)
getParameter n = do
  params <- gets functionDefinitionParameters
  let AST.Parameter ty name _ = params !! fromIntegral (natVal n)
  return . ValuePure $ AST.LocalReference ty name

tryGetParameter :: forall a ty . ValueOf a => Int -> FunctionDefinition ty (Maybe (Value 'Operand a))
tryGetParameter n = do
  params <- gets functionDefinitionParameters
  return $ do
    guard $ n < length params
    let AST.Parameter ty name _ = params !! n
    guard $ ty == valueType (Proxy :: Proxy a)
    return . ValuePure $ AST.LocalReference ty name
