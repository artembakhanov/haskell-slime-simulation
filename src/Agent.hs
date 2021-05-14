{-# LANGUAGE PatternSynonyms, GADTs, ViewPatterns, DeriveAnyClass, DeriveGeneric #-}

module Agent where

import Data.Array.Accelerate                              as A
import qualified Prelude                                  as P
import Data.Array.Accelerate.System.Random.MWC
import Constant                                           as C
import Data.Array.Accelerate.LLVM.Native                  as CPU
import Lib

data Agent = Agent_ {getIdx :: Int,
                     getX   :: Int,
                     getY   :: Int,
                     getA   :: Float}
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

-- | Create matrix from list of agent
-- * each element of matrix is the number of agents in this element
fromAgentsToMatrix :: Acc (Array DIM1 Agent) -> Acc (Matrix Float)
fromAgentsToMatrix agents =
    let
        zeros = fill (constant (Z:.width_:.height_)) 0
        ones  = fill (I1 (size agents))            1
    in
        permute (+) zeros (\ix -> Just_ (fromAgentToShape (agents!ix))) ones

-- | Initialize agents with random values
initAgents :: Int -> P.IO(Vector Agent)
initAgents n = do
    let
        idx = fromList (Z:.n) [0..] :: Vector Int
    x <- randomArray (uniformR (0, width_))   (Z :. n)           :: P.IO (Vector Int)
    y <- randomArray (uniformR (0, height_))  (Z :. n)           :: P.IO (Vector Int)
    a <- randomArray (uniformR (0, 2 * pi))    (Z :. n)          :: P.IO (Vector Float)
    P.return $ run $ (zipWith4 (\idx x y a -> Agent idx x y a) (use idx) (use x) (use y) (use a))

-- | Update coordinates of agents: move agents with by speed in some direction
moveAgents :: Exp Float -> Acc (Array DIM1 Agent) -> Acc (Array DIM1 Agent)
moveAgents time agents = map f agents
  where
    f (Agent idx x y a) = Agent idx x_ y_ a
      where
        x_, y_ :: Exp Int
        x_ = (x + round (speed * cos a)) `mod` width
        y_ = (y + round (speed * sin a)) `mod` height

randomBool :: Exp Float -> Exp Agent -> Exp Bool
randomBool time (Agent idx x y a) = (random time idx x y a) `mod` 2 == 0

random01 :: Exp Float -> Exp Agent -> Exp Float
random01 time (Agent idx x y a) = fromIntegral r / 4294967295.0
    where
        r = random time idx x y a

-- | Update directions of agents, agents go where they smell other agents most
updateAngles :: Exp Float -> Exp Float -> Acc (Array DIM2 Float) -> Acc (Array DIM1 Agent) -> Acc (Array DIM1 Agent)
updateAngles dt time trailMap agents = map f agents
  where
    f agent@(Agent idx x y a) = Agent idx x y a_
      where
        a_ :: Exp Float

        diffX, diffY :: Exp Int -> Exp Float -> Exp Int
        diffX x a = (x + round (dist * cos a)) `mod` width
        diffY y a = (y + round (dist * sin a)) `mod` height

        t0 = trailMap ! index2 (diffX x a)  (diffY y a)
        t1 = trailMap ! index2 (diffX x (a - rotation)) (diffY y (a - rotation))
        t2 = trailMap ! index2 (diffX x (a + rotation)) (diffY y (a + rotation))
        randAngle = random01 time agent
        a_ = ifThenElse (t0 > t1 && t0 > t2) 
                (a)
                (ifThenElse (t0 < t1 && t0 < t2)
                    (a + (randAngle - 0.5) * 2 * turnSpeed * dt)
                    (ifThenElse (t1 < t2)
                        (a - randAngle * turnSpeed * dt)
                        (a + randAngle * turnSpeed * dt)))

-- | Initialize trail map by 0 values
initTrailMap :: Array DIM2 Float
initTrailMap = run $ fill (constant (Z:.width_:.height_)) 0.0

-- | Updates trail map: blur + add new agents
updateTrailMap :: Exp Float -> Acc (Array DIM2 Float) -> Acc (Array DIM1 Agent) -> Acc (Array DIM2 Float)
updateTrailMap dt prev agents = newTrail
    where
        diffuseWeight = saturate diffuseRate * dt
        -- blur with diffuseWeight coefficient
        blurredTrail = map (\x -> diffuseWeight * x) (blur prev)
        prevWeighted = map (\x -> (1 - diffuseWeight) * x) prev
        sumTrail = permute (+) blurredTrail (\(I2 ix iy) -> Just_ (I2 ix iy)) prevWeighted
        -- add new agents to trail map
        ones = fill (I1 (size agents)) (trailWeight * dt)
        newTrail = permute (\x y -> min 1 (x + y)) sumTrail (\ix -> Just_ (fromAgentToShape (agents!ix))) ones