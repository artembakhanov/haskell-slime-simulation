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
import Prelude ((==))
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
    render world = scale 1 1 (bitmapOfArray (renderWorld world) False) -- draw agent
    update dt world = A.use (run (updateWorld dt world))               -- update world
  -- * Initialize agents by random values
  agents <- initAgents agentsNum
  GL.simulate
      (GL.InWindow "Cheliki" (width_, height_) (10, 20))
      black                                               -- background
      fps                                                 --
      (World agents initTrailMap (A.unit 0.0))            -- initial world
      render                                              -- draw world
      (\_ -> update)                              -- update world