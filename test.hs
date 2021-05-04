{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}



import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.IO.Codec.BMP                 as A
import Data.Array.Accelerate.LLVM.Native                  as CPU
import Data.Array.Accelerate.Data.Colour.RGB              as A
import Data.Array.Accelerate.Data.Colour.Names            as A
-- import Data.Vector										  as V

import qualified Prelude                                  as P
import Agent
-- data Agent = Agent (Vector Integer) Float

escapeToColor :: (Exp Float) -> Exp Colour
escapeToColor el = 
  rgb r g b
    where
      r = el / 2156 / 1680
      g = 1 - el / 2156 / 1680
      b = 0.8

main :: P.IO ()
main =
  let
      width   = 2156
      height  = 1680
      limit   = 1000
      radius  = 256
      test = fromList (Z:.width:.height) [0..] :: Matrix Float
      test1 = use test
      --
      img = A.map packRGB $ A.map escapeToColor $ test1
  in
  writeImageToBMP "test.bmp" (run img)
