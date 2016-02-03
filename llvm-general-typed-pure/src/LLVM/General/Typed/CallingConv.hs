{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.General.Typed.CallingConv where

import Data.Proxy
import GHC.TypeLits
import qualified LLVM.General.AST.CallingConvention as CC

-- |
-- A data kind for numbered calling conventions
data CallingConv where
  CallingConv :: Nat -> CallingConv

-- Define type aliases for some common calling conventions
type C            = 'CallingConv 0
type Fast         = 'CallingConv 8
type Cold         = 'CallingConv 9
type GHC          = 'CallingConv 10
type HiPE         = 'CallingConv 11
type X86_StdCall  = 'CallingConv 64
type X86_FastCall = 'CallingConv 65
type X86_64_Win64 = 'CallingConv 79

--
-- llvm-general has explicit constructors for some common
-- calling conventions, just as we have type aliases...
-- so we need to define mappings in both directions.
--

-- |
-- Reify a 'CallingConv' type witness into a 'CallingConvention' value
reifyCallingConv
  :: forall proxy nat . KnownNat nat
  => proxy ('CallingConv nat)
  -> CC.CallingConvention
reifyCallingConv _ = case natVal (Proxy :: Proxy nat) of
  0  -> CC.C
  8  -> CC.Fast
  9  -> CC.Cold
  10 -> CC.GHC
  x  -> CC.Numbered (fromIntegral x)

-- |
-- Promote a 'CallingConvention' value to a 'CallingConv' type
promoteCallingConv
  :: forall r
   . CC.CallingConvention
  -> (forall proxy nat . KnownNat nat => proxy ('CallingConv nat) -> r)
  -> Maybe r
promoteCallingConv cc f = case cc of
  CC.C    -> Just (f (Proxy :: Proxy C))
  CC.Fast -> Just (f (Proxy :: Proxy Fast))
  CC.Cold -> Just (f (Proxy :: Proxy Cold))
  CC.GHC  -> Just (f (Proxy :: Proxy GHC))
  CC.Numbered n -> do
    SomeNat witness <- someNatVal (fromIntegral n)
    return $ promoteNumbered witness f

promoteNumbered
  :: forall proxy nat r
   . proxy nat
  -> (Proxy ('CallingConv nat) -> r)
  -> r
promoteNumbered _ f = f Proxy
