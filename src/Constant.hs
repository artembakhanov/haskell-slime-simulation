module Constant where

import Data.Array.Accelerate                              as A
import qualified Prelude                                  as P
import Data.Array.Accelerate.System.Random.MWC

-- gaussian = [constant (0.06136),constant 0.24477, constant 0.38774, constant 0.24477, constant 0.06136]
gaussian :: [Exp Float]
gaussian = [constant 0.7, constant 0.8, constant 0.9, constant 0.8, constant 0.7]

-- trail coefficient
trailWeight, decayRate, dist, rotation, speed :: Exp Float
trailWeight = 0.3
decayRate = 0.25
dist = 4.0
rotation = pi / 6
speed = 2.5

width_, height_, fps, agentsNum :: Int
width_     =  800
height_    = 600
agentsNum  = 200000
fps        = 25

width, height :: Exp Int
width    = constant width_
height   = constant height_
