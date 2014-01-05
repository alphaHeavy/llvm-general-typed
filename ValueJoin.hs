{-# LANGUAGE GADTs #-}

module ValueJoin where

import BasicBlock
import Value

vjoin :: Value const a -> BasicBlock (Value const a)
vjoin (ValueOperand a) = a >>= return . ValueOperand . return
vjoin a = return a
