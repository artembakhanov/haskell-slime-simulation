{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE PatternSynonyms, GADTs, ViewPatterns, DeriveAnyClass, DeriveGeneric #-}

module Main where

import Constant
import Control.Exception (evaluate)
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
import System.Environment
import System.IO
import Data.List
import Constant

-- data State = State {getFps       :: Int,
--                     getWidth     :: A.Exp Int,
--                     getHeight    :: A.Exp Int,
--                     getAgentsNum :: Int
--                   }

-- initState = State fps width height agentsNum

dispatch :: [(String, String -> State -> State)]
dispatch =  [ ("fps",    changeFps),
              ("num",    changeAmount),
              ("width",  changeWidth),
              ("height", changeHeight)
            ]

applyArgs :: State -> [String] -> State
applyArgs state [] = state
applyArgs state (cmnd:val:args) = applyArgs newState args
  where
    value :: Int
    value    = read val
    action   = lookup cmnd dispatch
    newState = case action of 
      Nothing  -> state
      Just act   -> act val state

changeFps ::  String -> State -> State
changeFps val (State _ w h n) = State (read val) w h n 

changeWidth ::  String -> State -> State
changeWidth val  (State f _ h n) = State f (read val) h n 

changeHeight ::  String -> State -> State
changeHeight val (State f w _ n) = State f w ( read val) n 

changeAmount ::  String -> State -> State
changeAmount val (State f w h _) = State f w h (read val) 

-- changeAmount :: [String] -> IO ()
-- changeAmount (val:vals) = go initState

main = do
    args <- getArgs
    -- let (Just action) = lookup command dispatch
    let state = applyArgs initState args
    go state


go :: State -> IO ()
go state = do
  let
    State fps width height agentsNum = state
    render :: World -> Picture
    render world = scale 1 1 (bitmapOfArray (run1 (renderWorld) world) False) -- draw agent
    update dt world = updateWorld dt world                      -- update world
  -- * Initialize agents by random values
  -- print fps
  -- print agentsNum
  agents <- initAgents agentsNum
  initWorld <- evaluate (World_ agents initTrailMap (CPU.run $ A.unit 0.0))
  GL.simulate
      (GL.InWindow "Cheliki" (width, height) (10, 20))
      black                                               -- background
      fps                                                 --
      initWorld           -- initial world
      render                                              -- draw world
      (\_ -> run1 . update)                              -- update worldZ