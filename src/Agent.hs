{-# LANGUAGE PatternSynonyms, GADTs, ViewPatterns, DeriveAnyClass, DeriveGeneric #-}

module Agent where

import Data.Array.Accelerate                              as A
import qualified Prelude                                  as P
import Data.Array.Accelerate.System.Random.MWC
import Constant                                           as C

data Agent = Agent_ Int Int Int Float
    deriving (Generic, Elt)

pattern Agent :: Exp Int -> Exp Int -> Exp Int -> Exp Float -> Exp Agent
pattern Agent idx x y a = Pattern (idx, x, y, a)

type Stencil5x1 a = (Stencil3 a, Stencil5 a, Stencil3 a)
type Stencil1x5 a = (Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a)

convolve5x1 :: [Exp Float] -> Stencil5x1 Float -> Exp Float
convolve5x1 kernel (_, (a,b,c,d,e), _)
  = P.sum $ P.zipWith (*) kernel [a,b,c,d,e]

convolve1x5 :: [Exp Float] -> Stencil1x5 Float -> Exp Float
convolve1x5 kernel ((_,a,_), (_,b,_), (_,c,_), (_,d,_), (_,e,_))
  = P.sum $ P.zipWith (*) kernel [a,b,c,d,e]

blur :: Acc (Matrix Float) -> Acc (Matrix Float)
blur = stencil (convolve5x1 gaussian) clamp
     . stencil (convolve1x5 gaussian) clamp

fromAgentToShape :: Exp Agent -> Exp ((:.) ((:.) Z Int) Int)
fromAgentToShape (Agent idx x y v) = I2 x y

fromAgentsToMatrix :: Acc (Array DIM1 Agent) -> Acc (Matrix Float)
fromAgentsToMatrix agents =
    let
        zeros = fill (constant (Z:.width_:.height_)) 0
        ones  = fill (I1 (size agents))            1
    in
        permute (+) zeros (\ix -> Just_ (fromAgentToShape (agents!ix))) ones


initAgents :: Int -> P.IO(Acc(Vector Agent))
initAgents n = do
    let
        idx = fromList (Z:.n) [0..] :: Vector Int
    x <- randomArray (uniformR (0, width_))   (Z :. n)           :: P.IO (Vector Int)
    y <- randomArray (uniformR (0, height_))  (Z :. n)           :: P.IO (Vector Int)
    a <- randomArray (uniformR (0, 2 * pi))    (Z :. n)          :: P.IO (Vector Float)
    P.return (zipWith4 (\idx x y a -> Agent idx x y a) (use idx) (use x) (use y) (use a))


moveAgents :: Exp Float -> Acc (Array DIM1 Agent) -> Acc (Array DIM1 Agent)
moveAgents time agents = map f agents
  where
    f (Agent idx x y a) = Agent idx x_ y_ a
      where
        x_, y_ :: Exp Int
        x_ = (x + floor (2.5 * cos a)) `mod` width
        y_ = (y + floor (2.5 * sin a)) `mod` height

updateAngles :: Exp Float -> Exp Float -> Acc (Array DIM2 Float) -> Acc (Array DIM1 Agent) -> Acc (Array DIM1 Agent)
updateAngles dt time trailMap agents = map f agents
  where
    f (Agent idx x y a) = Agent idx x y a_
      where
        a_ :: Exp Float

        diffX, diffY :: Exp Int -> Exp Float -> Exp Int
        diffX x a = (x + floor (dist * cos a)) `mod` width
        diffY y a = (y + floor (dist * sin a)) `mod` height

        t0 = trailMap ! index2 (diffX x a)  (diffY y a)
        t1 = trailMap ! index2 (diffX x (a - rotation)) (diffY y (a - rotation))
        t2 = trailMap ! index2 (diffX x (a + rotation)) (diffY y (a + rotation))
        a_ = ifThenElse (t0 < t1) (ifThenElse (t1 < t2) (a + rotation) (a - rotation)) (ifThenElse (t0 > t2) (a) (a + rotation))

initTrailMap :: Acc (Array DIM2 Float)
initTrailMap = fill (constant (Z:.width_:.height_)) 0.0

updateTrailMap :: Exp Float -> Acc (Array DIM2 Float) -> Acc (Array DIM1 Agent) -> Acc (Array DIM2 Float)
updateTrailMap dt prev agents = newTrail
    where
        ones = fill (I1 (size agents)) trailWeight
        prev_ = map ((decayRate * dt) *) prev
        blurred_prev = blur prev_
        newTrail = permute (+) blurred_prev (\ix -> Just_ (fromAgentToShape (agents!ix))) ones
