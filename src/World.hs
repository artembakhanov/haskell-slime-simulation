{-# LANGUAGE PatternSynonyms, GADTs, ViewPatterns, DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE UndecidableInstances        #-}

module World where

import Constant
import Agent
import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.IO.Codec.BMP                           as A
import Data.Array.Accelerate.LLVM.Native                            as CPU
import qualified Data.Array.Accelerate.Data.Colour.RGB              as A
import qualified Data.Array.Accelerate.Data.Colour.Names            as A
import qualified Prelude                                            as P

-- | World consists of array of agents, trail map, total time
data World = World_ (A.Array A.DIM1 Agent) (A.Array A.DIM2 P.Float) (A.Scalar P.Float)
  deriving (A.Generic, Arrays)
-- | Pattern matching for Accelerate
pattern World :: A.Acc (A.Array A.DIM1 Agent) -> A.Acc (A.Array A.DIM2 Float) -> A.Acc (A.Array A.DIM0 P.Float) -> A.Acc World
pattern World {agents, trail, t } = A.Pattern (agents, trail, t)

-- | Draw pixel according to number of agents in this pixel and time that world exists
chelikToColor :: A.Exp Float -> A.Exp Float -> A.Exp A.Colour
chelikToColor _dt el =
  A.rgb r g b
    where
      r = el / 20.0 * 49.0 / 255.0
      g = el / 20.0 * 188.0 / 255.0
      b = el / 20.0 * 239.0 / 255.0

-- initWorld :: Int -> World


renderWorld :: Acc World -> Acc (A.Matrix A.Word32)
renderWorld (World agents trail t) =
  A.map A.packRGB $ A.map (chelikToColor (the t)) $ fromAgentsToMatrix agents

updateWorld :: Float -> A.Acc World -> A.Acc World
updateWorld dt world = (World rotatedCheliki newTrailMap (unit time))
    where
      (World agents trail t) = world
      time = (the t) + (constant dt)
      movedCheliki = moveAgents time agents
      rotatedCheliki = updateAngles (constant dt) time trail movedCheliki
      newTrailMap = updateTrailMap (constant dt) trail rotatedCheliki
