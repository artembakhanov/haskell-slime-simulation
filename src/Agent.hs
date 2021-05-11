module Agent where

import Data.Array.Accelerate                              as A
import qualified Prelude                                  as P
import Data.Array.Accelerate.System.Random.MWC
import Data.Array.Accelerate.Data.Bits                    as B
import System.IO.Unsafe

type Agent = ((Int, Int), Float)
type Stencil5x1 a = (Stencil3 a, Stencil5 a, Stencil3 a)
type Stencil1x5 a = (Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a)

convolve5x1 :: [Exp Float] -> Stencil5x1 Float -> Exp Float
convolve5x1 kernel (_, (a,b,c,d,e), _)
  = P.sum $ P.zipWith (*) kernel [a,b,c,d,e]

convolve1x5 :: [Exp Float] -> Stencil1x5 Float -> Exp Float
convolve1x5 kernel ((_,a,_), (_,b,_), (_,c,_), (_,d,_), (_,e,_))
  = P.sum $ P.zipWith (*) kernel [a,b,c,d,e]


-- gaussian = [constant (0.06136),constant 0.24477, constant 0.38774, constant 0.24477, constant 0.06136]
gaussian :: [Exp Float]
gaussian = [constant 0.4, constant 0.6, constant 0.9, constant 0.6, constant 0.4]

blur :: Acc (Matrix Float) -> Acc (Matrix Float)
blur = stencil (convolve5x1 gaussian) clamp
     . stencil (convolve1x5 gaussian) clamp


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
    f_ <- randomArray (uniformR (0, 2 * pi))    (Z :. n)         :: P.IO (Vector Float)

    P.return ( zip (zip (use x_) (use y_)) (use f_))


moveAgents :: (Exp Int, Exp Int, Exp Float) -> Acc (Array DIM1 Agent) -> Acc (Array DIM1 Agent)
moveAgents (width, height, dt) = map f
  where
    f agent = T2 (T2 x_ y_) v'
      where
        x    =  fst (fst agent)
        y    =  snd (fst agent)
        v    =  snd agent
        tmpX = x + round (2.0 * cos v)
        tmpY = y + round (2.0 * sin v)

        dt' :: Exp Int
        dt'       = round (dt*10000)
        tmprnd    = dt' `xor` 2747636419 * 2654435769 
        tmprnd2   = tmprnd  `xor`  (shift tmprnd 16 ) * 2654435769
        tmprnd3   = tmprnd2 `xor`  (shift tmprnd2 16 ) * 2654435769
        rnd       = 1--fromIntegral (tmprnd3 `div` 4294967295)

        condition1 = (tmpX >= width)  || (tmpX < 0)
        condition2 = (tmpY >= height) || (tmpY < 0)
        v'    = ifThenElse (condition1 || condition2) (2 * pi * rnd) v
        -- tmpX' = x + round (1.5 * cos v)
        -- tmpY' = y + round (1.5 * sin v)

        x_   = ifThenElse condition1 (min (width-1)  (max 0 tmpX))   tmpX
        y_   = ifThenElse condition2 (min (height-1) (max 0 tmpY))   tmpY
        -- x_ = (x + round (1.5 * cos v)) `mod` width
        -- y_ = (y + round (1.5 * sin v)) `mod` height
        v      :: Exp Float
        x_, y_ :: Exp Int

initTrailMap :: (Int, Int) -> Acc (Array DIM2 Float)
initTrailMap (width, height) = fill (constant (Z:.width:.height)) 0.0


updateTrailMap :: Acc (Array DIM2 Float) -> Acc (Array DIM1 Agent) -> Acc (Array DIM2 Float)
updateTrailMap prev agents = new
    where
        ones = fill (I1 (size agents)) 1.0
        prev_ = map ((0.25 * 0.03) *) prev
        blurred_prev = blur prev_
        new = permute (+) blurred_prev (\ix -> Just_ (fromAgentToShape (agents!ix))) ones

updateAngles :: (Exp Int, Exp Int) -> Acc (Array DIM1 Agent) -> Acc (Array DIM2 Float) -> Acc (Array DIM1 Agent)
updateAngles (width, height) agents trailMap = map f agents
  where
    f agent = T2 (T2 x y) v_
      where
        dist = 4.0
        x, y :: Exp Int
        v, v_ :: Exp Float
        x =  fst (fst agent)
        y =  snd (fst agent)
        v = snd agent

        diffX, diffY :: Exp Int -> Exp Float -> Exp Int
        diffX x v = (x + round (dist * cos v)) `mod` width
        diffY y v = (y + round (dist * sin v)) `mod` height
--        t0, t1, t2 :: Exp(Float)
        t0 = trailMap ! index2 (diffX x v)  (diffY y v)
        t1 = trailMap ! index2 (diffX x (v - rotation)) (diffY y (v - rotation))
        t2 = trailMap ! index2 (diffX x (v + rotation)) (diffY y (v + rotation))
        v_ = ifThenElse (t0 < t1) (ifThenElse (t1 < t2) (v + rotation) (v - rotation)) (ifThenElse (t0 > t2) (v) (v + rotation))


        rotation = pi / 6