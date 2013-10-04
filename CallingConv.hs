{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module CallingConv where

import GHC.TypeLits

data CallingConv where
  CallingConv :: Nat -> CallingConv

type C            = 'CallingConv 0
type Fast         = 'CallingConv 8
type Cold         = 'CallingConv 9
type GHC          = 'CallingConv 10
type HiPE         = 'CallingConv 11
type X86_StdCall  = 'CallingConv 64
type X86_FastCall = 'CallingConv 65
type X86_64_Win64 = 'CallingConv 79

