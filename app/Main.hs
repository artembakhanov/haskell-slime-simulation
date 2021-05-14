{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE PatternSynonyms, GADTs, ViewPatterns, DeriveAnyClass, DeriveGeneric #-}

module Main where

import Constant
import Agent
import World
import Prelude                                                      as P
import Data.Array.Accelerate.LLVM.Native                            as CPU
import qualified Data.Array.Accelerate                              as A
import qualified Data.Array.Accelerate.IO.Codec.BMP                 as A
import qualified Data.Array.Accelerate.Data.Colour.RGB              as A
import qualified Data.Array.Accelerate.Data.Colour.Names            as A
import qualified Graphics.Gloss                                     as GL
import Graphics.Gloss (Picture, blank, scale)
import Graphics.Gloss.Data.Color (black)
import Graphics.Gloss.Accelerate.Data.Picture (bitmapOfArray)

main :: IO ()
main = do
  let
    -- render = draw zoom . CPU.run1 (colourise)
    draw :: Float -> A.Matrix A.Word32 -> Picture
    draw zoom arr = scale zoom zoom (bitmapOfArray arr False)
    render :: World -> Picture
    render = draw 1 . CPU.run1 renderWorld -- draw agent
    update dt world = A.use (run (updateWorld dt world))               -- update world
  -- * Initialize agents by random values
  agents <- (initAgents agentsNum)
  let initAgents =  CPU.run agents
  let initWorld = CPU.run initTrailMap
  GL.simulate
      (GL.InWindow "Cheliki" (width_, height_) (10, 20))
      black                                               -- background
      fps                                                 --
      (World_ initAgents initWorld (CPU.run $ A.unit 0.0))            -- initial world
      render                                              -- draw world
      (\_ dt -> CPU.run1 (updateWorld dt))                              -- update world