module Constant where

import Data.Array.Accelerate                              as A
import qualified Prelude                                  as P
import Data.Array.Accelerate.System.Random.MWC ()

data State = State {getFps       :: Int,
                    getWidth     :: Int,
                    getHeight    :: Int,
                    getAgentsNum :: Int
                  }
                  
-- gaussian = [constant (0.06136),constant 0.24477, constant 0.38774, constant 0.24477, constant 0.06136]
gaussian :: [Exp Float]
gaussian = [constant 0.7, constant 0.8, constant 0.9, constant 0.8, constant 0.7]

-- trail coefficient
trailWeight, decayRate, dist, rotation, speed, diffuseRate :: Exp Float
trailWeight = 5
decayRate = 0.2
dist = 4.0
rotation = pi / 6
speed = 10
diffuseRate = 3.0

width_, height_, fps, agentsNum :: Int
width_     = 200
height_    = 200
agentsNum  = 200000
fps        = 25

width, height :: Exp Int
width    = constant width_
height   = constant height_

initState = State fps width_ height_ agentsNum