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

chelikiToColor :: (Exp Float) -> Exp Colour
chelikiToColor el =
  rgb r g b
    where
      r = el
      g = el
      b = el

main :: P.IO ()
main =
  let
      width   = 2156
      height  = 1680
      limit   = 1000
      radius  = 256
      test = fromList (Z:.width:.height) [0..] :: Matrix Float
      test1 = use test
      a = initAgents 1000000 (width, height)
      testCheliki = fromAgentsToMatrix (width, height) a
      --
      img = A.map packRGB $ A.map escapeToColor $ test1
      imgCheliki = A.map packRGB $ A.map chelikiToColor $ testCheliki
  in
  writeImageToBMP "test.bmp" (run imgCheliki)
