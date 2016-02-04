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
-- A 'Function' is a wrapper around a 'Constant' 'Value' tagged with a specific 'CallingConv'.
--
-- @
-- 'Function' 'LLVM.General.Typed.CallingConv.C' ('Data.Int.Int64' 'Prelude.->' 'Data.Int.Int32') = 'Value' 'Constant' ('Ptr' ('Data.Int.Int64' 'Prelude.->' 'Data.Int.Int32'))
-- 'Function' 'LLVM.General.Typed.CallingConv.X86_StdCall' ('Foreign.C.CString.CString' 'Prelude.->' 'Data.Int.Int32') = 'Value' 'Constant' ('Ptr' ('Foreign.C.CString.CString' 'Prelude.->' 'Data.Int.Int32')) @
data Function (cconv :: CallingConv) (a :: *) where
  Function :: Value 'Constant (Ptr a) -> CC.CallingConvention -> Function ('CallingConv nat) a

-- |
-- Create a 'Function' from a constant pointer.
createFunction
  :: forall a nat
   . KnownNat nat
  => Value 'Constant (Ptr a)
  -> Function ('CallingConv nat) a
createFunction value =
  let cc = reifyCallingConv (Proxy :: Proxy ('CallingConv nat))
  in Function value cc

-- |
-- Return the address of a given function.
functionValue
  :: Function cconv a
  -> Value 'Constant (Ptr a)
functionValue (Function value _) = value

-- |
-- Return the calling convention of a given function.
functionCallingConv
  :: Function cconv a
  -> CC.CallingConvention
functionCallingConv (Function _ cconv) = cconv

-- |
-- Get the parameter at the location given by a 'Nat' witness.
-- The type at this location must be equal to the type of the
-- 'Value' requested. This property is enforced by indexing into
-- the function type list and asserting type equivalence.
getParameter
  :: (ParameterType ty n ~ a, KnownNat n)
  => proxy n -- ^ An index witness such as @ 'Proxy' :: 'Proxy' 0 @
  -> FunctionDefinition ty (Value 'Operand a)
getParameter n = do
  params <- gets functionDefinitionParameters
  let AST.Parameter ty name _ = params !! fromIntegral (natVal n)
  return . ValuePure $ AST.LocalReference ty name

-- |
-- Get the parameter at the location given by an 'Int'.
-- Unlike 'getParameter', this function does all type and index
-- checks at runtine and will fail if the types are not equivalent
-- or if the requested index is out of bounds.
tryGetParameter
  :: forall a ty
   . ValueOf a
  => Int -- ^ The parameter index
  -> FunctionDefinition ty (Maybe (Value 'Operand a))
tryGetParameter n = do
  params <- gets functionDefinitionParameters
  return $ do
    -- simple bounds check
    guard $ n >= 0 && n < length params
    let AST.Parameter ty name _ = params !! n
    guard $ ty == valueType (Proxy :: Proxy a)
    return . ValuePure $ AST.LocalReference ty name
