module Lib
  ( random
  ) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Bits                              as B
import qualified Prelude                                            as P
import Agent


a1, a2, a3, a4, a5 :: Exp Int
a1 = 6571
a2 = 31
a3 = 4327
a4 = 6323
a5 = 2707

random :: Exp Float -> Exp Int -> Exp Int -> Exp Int -> Exp Float -> Exp Word32
random time idx x y a = state3
    where
        state = fromIntegral ((round time * a1) + idx * a2 + x * a3 + y * a4 + round (a * 2707))

        state1 = (state              `xor` 2747636419)   * 2654435769
        state2 = ((shift state1 16)  `xor` state1)        * 2654435769
        state3 = ((shift state2 16)  `xor` state2)        * 2654435769


randomBool :: Exp Float -> Exp Agent -> Exp Bool
randomBool time (Agent idx x y a) = (random time idx x y a) `mod` 2 == 0
  