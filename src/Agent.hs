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
initAgents :: Int -> (Int, Int) -> Acc (Vector Agent)
initAgents n (width, height) = zip (zip x_ y_) f_
    where
        x_ = use (unsafePerformIO x_')
        y_ = use (unsafePerformIO y_')
        f_ = use (unsafePerformIO f_')
        x_' = randomArray (uniformR (0, width))  (Z :. n)          :: P.IO (Vector Int)
        y_' = randomArray (uniformR (0, height))  (Z :. n)         :: P.IO (Vector Int)
        f_' = randomArray (uniformR (0, pi*2))  (Z :. n)           :: P.IO (Vector Float)
        -- a  = P.fromIntegral (n `P.mod` 100)
        -- x_ = P.floor (a P./ 100 P.* P.fromIntegral (width))
        -- y_ = P.floor (a P./ 100 P.* P.fromIntegral (height))
-- initAgents 0 (width, height) = []
-- initAgents n (width, height) = ((x_, y_), 0) : initAgents (n - 1) (width, height)
--     where
--         a  = P.fromIntegral (n `P.mod` 100)
--          x_ = P.floor (a P./ 100 P.* P.fromIntegral (width))
--         y_ = P.floor (a P./ 100 P.* P.fromIntegral (height))