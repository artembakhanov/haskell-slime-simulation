module Constant where

import Data.Array.Accelerate                              as A
import qualified Prelude                                  as P
import Data.Array.Accelerate.System.Random.MWC

-- gaussian = [constant (0.06136),constant 0.24477, constant 0.38774, constant 0.24477, constant 0.06136]
-- gaussian :: [Exp Float]
-- gaussian = [constant 0.7, constant 0.8, constant 0.9, constant 0.8, constant 0.7]

gaussian :: [Exp Float]
gaussian = [constant 0.2, constant 0.2, constant 0.2, constant 0.2, constant 0.2]

-- trail coefficient
trailWeight, decayRate, dist, rotation, speed, diffuseRate :: Exp Float
trailWeight = 5
decayRate = 0.2
dist = 15
rotation = pi / 6
speed = 10
diffuseRate = 3.0

width_, height_, fps, agentsNum :: Int
width_     = 1080
height_    = 1920
agentsNum  = 250000
fps        = 30

width, height :: Exp Int
width    = constant width_
height   = constant height_
