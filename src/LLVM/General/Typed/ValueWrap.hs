{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.General.Typed.ValueWrap where

import Control.Monad
import Data.Int
import Data.Typeable
import Data.Void
import Data.Word
import Foreign.Ptr (Ptr)
import GHC.TypeLits
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant
import qualified LLVM.General.AST.Float as Float
import qualified LLVM.General.AST.Global as AST

import LLVM.General.Typed.BlockAddress
import LLVM.General.Typed.CallingConv
import LLVM.General.Typed.Function
import LLVM.General.Typed.Value

matchingTypes :: AST.Type -> TypeRep -> Bool
matchingTypes typ rep = rep `elem` table ((:[]) . typeRep) typ where
  table :: (forall a proxy. Typeable a => proxy (a :: *) -> [TypeRep]) -> AST.Type -> [TypeRep]
  table f (AST.IntegerType  8) = f (Proxy :: Proxy  Int8) ++ f (Proxy :: Proxy  Word8)
  table f (AST.IntegerType 16) = f (Proxy :: Proxy Int16) ++ f (Proxy :: Proxy Word16)
  table f (AST.IntegerType 32) = f (Proxy :: Proxy Int32) ++ f (Proxy :: Proxy Word32)
  table f (AST.IntegerType 64) = f (Proxy :: Proxy Int64) ++ f (Proxy :: Proxy Word64)
  table _ (AST.IntegerType  _) = []
  table f (AST.FloatingPointType 32 _) = f (Proxy :: Proxy Float)
  table f (AST.FloatingPointType 64 _) = f (Proxy :: Proxy Double)
  table _ (AST.FloatingPointType  _ _) = []
  table f AST.VoidType = f (Proxy :: Proxy Void)
  table f (AST.PointerType typ' _) = table g typ' where
    g :: forall a proxy . Typeable a => proxy a -> [TypeRep]
    g _ = f (Proxy :: Proxy (Ptr a))
{-
 - no Typeable instance for Nats, need to figure out a different mechanism
  table f (AST.ArrayType n typ') = table g typ' where
    g :: forall a proxy . Typeable a => proxy a -> [TypeRep]
    g _ = case someNatVal (fromIntegral n) of
      Just (SomeNat (_ :: Proxy n')) -> f (Proxy :: Proxy (Array n' a))
-}

class OperandWrap const where
  operandWrap :: Typeable a => AST.Operand -> Maybe (Value const a)

instance OperandWrap Constant where
  operandWrap :: forall a. Typeable a => AST.Operand -> Maybe (Value Constant a)
  operandWrap (AST.ConstantOperand op) = do
    let rep = typeOf (Proxy :: Proxy a)
        val = ValueConstant op
    case op of
      Constant.Int n _ | matchingTypes (AST.IntegerType n) rep -> Just val
      Constant.Float (Float.Single _) | rep == typeOf (Proxy :: Proxy  Float) -> Just val
      Constant.Float (Float.Double _) | rep == typeOf (Proxy :: Proxy Double) -> Just val
      Constant.Null  typ | matchingTypes typ rep -> Just val
      Constant.Undef typ | matchingTypes typ rep -> Just val
      Constant.Array typ _xs | matchingTypes typ rep -> Just val
      Constant.Struct _ _packed _xs -> error "Constant.Struct not implemented"
      Constant.BlockAddress{} | rep == typeOf (Proxy :: Proxy BlockAddress) -> Just val
      Constant.GlobalReference{} -> error "Constant.GlobalReference not implemented"

functionWrap :: forall a cc . (KnownNat cc, Typeable a) => AST.Global -> Maybe (Function ('CallingConv cc) a)
functionWrap AST.Function{..} = do
  guard (callingConvention == reifyCallingConv (Proxy :: Proxy ('CallingConv cc)))
  undefined
functionWrap _ = Nothing
