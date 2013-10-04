{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Function where

import CallingConv

data Function (cconv :: CallingConv) (a :: *)
