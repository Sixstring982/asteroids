module Asteroid (
  Asteroid(Asteroid),
  Asteroids,
  size,
  asteroidPosition,
  generateInitial,
  Asteroid.update,
  Asteroid.render,
) where

import System.Random
import Graphics.Gloss
import Bullet
import Math
import Screen
import Vector2

data Asteroid = Asteroid { angle :: Float,
                           size :: Float,
                           pos :: Vector2,
                           vel :: Vector2,
                           shape :: [Vector2]
                         } deriving(Show, Eq)

type Asteroids = [Asteroid]

maxAsteroidSize :: Float
maxAsteroidSize = 30.0

minAsteroidSize :: Float
minAsteroidSize = 5.0

maxVelocity :: Float
maxVelocity = 5.0

asteroidArmCount :: Int
asteroidArmCount = 8

rotationRate :: Float
rotationRate = 3.0

initialAsteroidCount :: Int
initialAsteroidCount = 10

asteroidPosition :: Asteroid -> Vector2
asteroidPosition = pos

generateInitial :: Int -> Asteroids
generateInitial n = generateAsteroids n initialAsteroidCount maxAsteroidSize

generateAsteroids :: Int -> Int -> Float -> Asteroids
generateAsteroids n num s =
  let rand = mkStdGen n in
  let (as, _) =
        foldl (\ (ls, g) n -> let (a, new_g) = generateAsteroid s g in
                              (a : ls, new_g))
              ([], rand)
              [1..num]
  in as

generateRadius :: RandomGen g => Float -> g -> (Float, g)
generateRadius s = randomR (0.5 * s, s)

generateArms :: RandomGen g => Float -> g -> ([Vector2], g)
generateArms s g0 =
  foldl (\ (ls, g) n -> let (r, gn) = generateRadius s g in
                        let theta = ((fromIntegral n) * pi * 2.0) /
                                    ((fromIntegral asteroidArmCount)) in
                        ((fromPolar (theta, r)) : ls, gn))
        ([], g0)
        [1..asteroidArmCount]

generatePosition :: RandomGen g => g -> (Vector2, g)
generatePosition g0 = ((Vector2 x y), g2) where
  (w, h) = Screen.dimensions
  (iw, ih) = ((fromIntegral w) / 2, (fromIntegral h) / 2)
  (x, g1) = randomR (-iw, iw) g0
  (y, g2) = randomR (-ih, ih) g1

generateVelocity :: RandomGen g => g -> (Vector2, g)
generateVelocity = randomNormalized

generateAsteroid :: RandomGen g => Float -> g -> (Asteroid, g)
generateAsteroid s g0 = (Asteroid 0 s pos vel arms, g3) where
  (pos, g1)  = generatePosition g0
  (vel, g2)  = generateVelocity g1
  (arms, g3) = generateArms s g2

pictureFromAsteroid :: Asteroid -> Picture
pictureFromAsteroid (Asteroid a _ (Vector2 x y) _ vs) =
  translate x y $ rotate (degFromRad a) $ toLineLoop vs

render :: Asteroids -> Picture
render as = Pictures [pictureFromAsteroid a | a <- as]

splitAsteroid :: Int -> Asteroid -> Asteroids
splitAsteroid n a = map (\b -> b { pos = pos a }) split_asteroids where
  smaller_asteroids = generateAsteroids n 2 (size a / 2)
  split_asteroids   = filter (\b -> size b > minAsteroidSize) smaller_asteroids

updateRotation :: Float -> Asteroid -> Asteroid
updateRotation f a@(Asteroid t _ _ _ _) = a { angle = t + f * rotationRate }

updatePosition :: Float -> Asteroid -> Asteroid
updatePosition f a@(Asteroid _ _ p v _) = a { pos = Vector2 nx ny } where
  (w, h) = Screen.dimensions
  (fw, fh) = ((fromIntegral w), (fromIntegral h))
  (hw, hh) = (fw / 2, fh / 2)
  (Vector2 x y) = p + v
  nx = if x < (-hw) then x + fw else if x > hw then x - fw else x
  ny = if y < (-hh) then y + fh else if y > hh then y - fh else y

updateAsteroid :: Float -> Asteroid -> Asteroid
updateAsteroid f = (updateRotation f) . (updatePosition f)

scorePerHit :: Int
scorePerHit = 50

scorePerLevel :: Int
scorePerLevel = 1000

update :: Int -> Int -> Float -> Asteroids -> Bullets -> (Asteroids, Bullets, Int)
update n score f as bs = (new_asteroids, new_bullets, new_score) where
  collisions        = [(a, b) | a <- as, b <- bs, distance (bulletPos b) (pos a) < size a]
  dead_bullets      = map snd collisions
  hit_asteroids     = map fst collisions
  through_asteroids = filter (\a -> not (a `elem` hit_asteroids)) as
  split_asteroids   = concatMap (splitAsteroid n) hit_asteroids
  all_asteroids     = concat [split_asteroids, through_asteroids]
  new_asteroids     = if length all_asteroids == 0 then generateInitial n
                      else map (updateAsteroid f) all_asteroids
  new_bullets       = filter (\b -> not (b `elem` dead_bullets)) bs
  new_score         = score + scorePerHit * length dead_bullets +
                      if length all_asteroids == 0 then scorePerLevel else 0
