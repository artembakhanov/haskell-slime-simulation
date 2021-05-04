module Agent
    (Agent) where

import Data.Array.Accelerate                              as A
import qualified Prelude                                  as P

data Agent = Agent (Vector (Int)) Float
type AgentElt = (Int, Int, Float)


fromAgentsToMatrix :: (width, height) -> Acc (Vector AgentElt) -> Acc (Matrix Float)
fromAgentsToMatrix (width, height) Z = fromList (Z:.width:.height) (repeat 0)
fromAgentsToMatrix (width, height) agents = _
  where
    matrix = fold (op) 
    matrix[] = 

initAgentsVector n (width, height) = 
  fromList (Z:.n) (initAgentsElt n (width, height))


initAgentsElt :: Int -> (Int, Int) -> [AgentElt]
initAgentsElt 0 (width, height) = []
initAgentsElt n (width, height) = 
  (x_, y_, 0) : initAgentsElt (n - 1) (width, height)
  	where
      a  = P.fromIntegral (n `P.mod` 100)
      x_ = P.floor (a P./ 100 P.* P.fromIntegral (width))
      y_ = P.floor (a P./ 100 P.* P.fromIntegral (height))

initAgents :: Int -> (Int, Int) -> [Agent]

initAgents 0 (width, height) = []
initAgents n (width, height) = 
  (Agent (fromList (Z:.2) [x_, y_]) 0) : initAgents (n - 1) (width, height)
    where
      a  = P.fromIntegral (n `P.mod` 100)
      x_ = P.floor (a P./ 100 P.* P.fromIntegral (width))
      y_ = P.floor (a P./ 100 P.* P.fromIntegral (height))