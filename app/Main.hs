{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE PatternSynonyms, GADTs, ViewPatterns, DeriveAnyClass, DeriveGeneric #-}

module Main where

import Constant
import Agent
import qualified Data.Array.Accelerate                              as A
import qualified Data.Array.Accelerate.IO.Codec.BMP                 as A
import Data.Array.Accelerate.LLVM.Native                            as CPU
import qualified Data.Array.Accelerate.Data.Colour.RGB              as A
import qualified Data.Array.Accelerate.Data.Colour.Names            as A
import qualified Graphics.Gloss as GL
import Graphics.Gloss (Picture, blank, scale)
import Graphics.Gloss.Data.Color (black)
import Prelude ((==))
import Prelude                                                      as P
import Graphics.Gloss.Accelerate.Data.Picture (bitmapOfArray)

data World = World_ (A.Array A.DIM1 Agent) (A.Array A.DIM2 Float)
  deriving (A.Generic, Arrays)

pattern World :: Acc (A.Array A.DIM1 Agent) -> Acc (A.Array A.DIM2 Float) -> Acc World
pattern World { agents, trail } = A.Pattern (agents, trail)

chelikiToColor :: A.Exp Float -> A.Exp A.Colour
chelikiToColor el =
  A.rgb r g b
    where
      r = el / 20.0 * 49.0 / 255.0
      g = el / 20.0 * 188.0 / 255.0
      b = el / 20.0 * 239.0 / 255.0

render :: (Int, Int) -> Acc World ->  Acc (A.Matrix Float) -- Picture
render (width, height) (World agents trail) = cheliki
--  scale 1 1 (bitmapOfArray (run imgCheliki) False)
    where
      cheliki = fromAgentsToMatrix (width, height) agents
--    imgCheliki = A.map A.packRGB $ A.map chelikiToColor $ cheliki

update :: (Int, Int) -> Acc World -> Acc World
update (width, height) (World agents trail) = (World rotatedCheliki newTrailMap)
    where
    cheliki = fromAgentsToMatrix (width, height) agents
    movedCheliki = moveAgents (A.constant width, A.constant height) agents
    rotatedCheliki = (updateAngles (A.constant width, A.constant height) movedCheliki trail)
    newTrailMap = (updateTrailMap trail rotatedCheliki)


main :: IO ()
main = do
  let
    trailMap = initTrailMap (width, height)
    rndr world  = scale 1 1 (bitmapOfArray (imgCheliki) False)
       where
         imgCheliki = run $ A.map A.packRGB $ A.map chelikiToColor $ render (width, height) world
    upd world = A.use (run (update (width, height) world))
  agents <- initAgents 2000000 (width, height)
  GL.simulate
      (GL.InWindow "Cheliki" (width, height) (10, 20))
      black
      fps
      (World agents trailMap)
      rndr --(return (render (width, height) world trailMap))
      (\_ _dt -> upd)
  -- loop 200 (width, height) world trailMap
