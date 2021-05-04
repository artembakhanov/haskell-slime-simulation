{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

import Agent
import qualified Data.Array.Accelerate                              as A
import qualified Data.Array.Accelerate.IO.Codec.BMP                 as A
import Data.Array.Accelerate.LLVM.Native                  as CPU
import qualified Data.Array.Accelerate.Data.Colour.RGB              as A
import qualified Data.Array.Accelerate.Data.Colour.Names            as A
import Prelude ((==))
import Prelude                                            as P

chelikiToColor :: (A.Exp Float) -> A.Exp A.Colour
chelikiToColor el =
  A.rgb r g b
    where
      r = el
      g = el
      b = el

loop :: Int -> (Int, Int) -> Acc (A.Array A.DIM1 Agent) -> Acc (A.Array A.DIM2 Float) -> IO()
loop 0 (_, _) _ _ = do print "Done!"
loop n (width, height) a trail = do
  let
    cheliki = fromAgentsToMatrix (width, height) a

    movedCheliki = moveAgents ((A.constant width), (A.constant height)) a
    rotatedCheliki = updateAngles ((A.constant width), (A.constant height)) movedCheliki trail
    newTrailMap = updateTrailMap trail rotatedCheliki
    imgCheliki = A.map A.packRGB $ A.map chelikiToColor $ cheliki
  A.writeImageToBMP ("test" P.++ (show n) P.++ ".bmp") (run imgCheliki)
  loop (n - 1) (width, height) rotatedCheliki newTrailMap

main :: IO ()
main = do
  let
    width = 600
    height  = 600
    trailMap = initTrailMap (width, height)
  a <- initAgents 5000 (width, height)
  loop 50 (width, height) a trailMap
