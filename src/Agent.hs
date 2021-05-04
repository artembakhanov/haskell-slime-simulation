module Agent where

import Data.Array.Accelerate                              as A
import qualified Prelude                                  as P

type Agent = ((Int, Int), Float)

fromAgentToShape :: Exp Agent -> Exp ((:.) ((:.) Z Int) Int)
fromAgentToShape agent = I2 (fst (fst agent)) (snd (fst agent))

fromAgentsToMatrix :: (Int, Int) -> Acc (Array DIM1 Agent) -> Acc (Matrix Float)
fromAgentsToMatrix (width, height) agents =
    let
        zeros = fill (constant (Z:.width:.height)) 0
        ones  = fill (I1 (size agents))            1          -- which shape is it?
    in
        permute (+) zeros (\ix -> Just_ (fromAgentToShape (agents!ix))) ones

initAgentsElt n (width, height) = fromList (Z:.n) (initAgents n (width, height))

initAgents :: Int -> (Int, Int) -> [Agent]
initAgents 0 (width, height) = []
initAgents n (width, height) = ((x_, y_), 0) : initAgents (n - 1) (width, height)
    where
        a  = P.fromIntegral (n `P.mod` 100)
        x_ = P.floor (a P./ 100 P.* P.fromIntegral (width))
        y_ = P.floor (a P./ 100 P.* P.fromIntegral (height))