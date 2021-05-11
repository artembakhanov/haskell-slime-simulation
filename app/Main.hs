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
import qualified Graphics.Gloss as GL
import Graphics.Gloss (Picture, blank, scale)
import Graphics.Gloss.Data.Color (black)
import Prelude ((==))
import Prelude                                            as P
import Graphics.Gloss.Accelerate.Data.Picture (bitmapOfArray)

chelikiToColor :: A.Exp Float -> A.Exp A.Colour
chelikiToColor el =
  A.rgb r g b
    where
      r = el / 20.0 * 49.0 / 255.0
      g = el / 20.0 * 188.0 / 255.0
      b = el / 20.0 * 239.0 / 255.0

render :: (Int, Int) -> Acc (A.Array A.DIM1 Agent) -> Acc (A.Array A.DIM2 Float)
   -> Picture
render (width, height) a trail = --do
  scale 1 1 (bitmapOfArray (run imgCheliki) False)
    where
    cheliki = fromAgentsToMatrix (width, height) a
    -- movedCheliki = moveAgents (A.constant width, A.constant height) a
    -- rotatedCheliki = A.use (run (updateAngles (A.constant width, A.constant height) movedCheliki trail))
    -- newTrailMap = A.use (run (updateTrailMap trail rotatedCheliki))
    imgCheliki = A.map A.packRGB $ A.map chelikiToColor $ cheliki

update :: (Int, Int) -> Acc (A.Array A.DIM1 Agent) -> Acc (A.Array A.DIM2 Float) -> Float
  -> (Acc (A.Vector Agent), Acc (A.Array A.DIM2 Float), Float)
update (width, height) a trail dt = ( rotatedCheliki, newTrailMap, dt + 0.001)
    where
    cheliki = fromAgentsToMatrix (width, height) a
    movedCheliki = moveAgents (A.constant width, A.constant height, A.constant dt) a
    rotatedCheliki = A.use (run (updateAngles (A.constant width, A.constant height) movedCheliki trail))
    newTrailMap = A.use (run (updateTrailMap trail rotatedCheliki))


main :: IO ()
main = do
  let
    width    = 1080 `div` 2
    height   = 1920 `div` 2
    fps      = 15
    trailMap = initTrailMap (width, height)
    rndr     = \ (x,y,_) -> render (width, height) x y
    update'  = \ (x,y,z) -> update (width, height) x y z
  world <- initAgents 1000000 (width, height)
  GL.simulate
      (GL.InWindow "Cheliki" (height, width) (10, 20))
      black
      fps
      (world, trailMap, 0.1)
      rndr --(return (render (width, height) world trailMap))
      (\_ _dt -> update')
  -- loop 200 (width, height) world trailMap
