module Agent where

import Data.Array.Accelerate                              as A
import qualified Prelude                                  as P
import Data.Array.Accelerate.System.Random.MWC
import System.IO.Unsafe

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

-- initAgentsElt n (width, height) = fromList (Z:.n) (initAgents n (width, height))
-- 
initAgents :: Int -> (Int, Int) -> P.IO(Acc (Vector Agent))
initAgents n (width, height) = do
    x_ <- randomArray (uniformR (0, width))   (Z :. n)           :: P.IO (Vector Int)
    y_ <- randomArray (uniformR (0, height))  (Z :. n)           :: P.IO (Vector Int)
    f_ <- randomArray (uniformR (0, pi*2))    (Z :. n)           :: P.IO (Vector Float)

    P.return ( zip (zip (use x_) (use y_)) (use f_))
        -- a  = P.fromIntegral (n `P.mod` 100)
        -- x_ = P.floor (a P./ 100 P.* P.fromIntegral (width))
        -- y_ = P.floor (a P./ 100 P.* P.fromIntegral (height))
-- initAgents 0 (width, height) = []
-- initAgents n (width, height) = ((x_, y_), 0) : initAgents (n - 1) (width, height)
--     where
--         a  = P.fromIntegral (n `P.mod` 100)
--          x_ = P.floor (a P./ 100 P.* P.fromIntegral (width))
--         y_ = P.floor (a P./ 100 P.* P.fromIntegral (height))

moveAgents :: Acc (Array DIM1 Agent) -> Acc (Array DIM1 Agent)
moveAgents agents = map f agents
  where
    f agent = T2 (T2 x_ y_) v
      where
        x =  (fst (fst agent))
        y =  (snd (fst agent))
        v :: Exp (Float)
        v =  (snd agent)
        x_, y_ :: Exp (Int)
        x_ = x + 15 * floor (cos v)
        y_ = y + 15 * floor (sin v)

