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
                           shape :: [Vector2],
                           alive :: Bool
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

generateInitial :: Asteroids
generateInitial = generateAsteroids 1024 initialAsteroidCount maxAsteroidSize

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
generatePosition g0 =
  let (w, h) = Screen.dimensions in
  let (iw, ih) = ((fromIntegral w) / 2, (fromIntegral h) / 2) in
  let (x, g1) = randomR (-iw, iw) g0 in
  let (y, g2) = randomR (-ih, ih) g1 in
    ((Vector2 x y), g2)

generateVelocity :: RandomGen g => g -> (Vector2, g)
generateVelocity = randomNormalized

generateAsteroid :: RandomGen g => Float -> g -> (Asteroid, g)
generateAsteroid s g0 =
  let (pos, g1)  = generatePosition g0 in
  let (vel, g2)  = generateVelocity g1 in
  let (arms, g3) = generateArms s g2 in
  (Asteroid 0 s pos vel arms True, g3)

pictureFromAsteroid :: Asteroid -> Picture
pictureFromAsteroid (Asteroid a _ (Vector2 x y) _ vs _) =
  translate x y $ rotate (degFromRad a) $ toLineLoop vs

render :: Asteroids -> Picture
render as = Pictures [pictureFromAsteroid a | a <- as]

splitAsteroid :: Int -> Asteroid -> Asteroids
splitAsteroid n a = map (\b -> b { pos = pos a }) split_asteroids where
  smaller_asteroids = generateAsteroids n 2 (size a / 2)
  split_asteroids   = filter (\b -> size b > minAsteroidSize) smaller_asteroids

updateRotation :: Float -> Asteroid -> Asteroid
updateRotation f a@(Asteroid t _ _ _ _ _) = a { angle = t + f * rotationRate }

updatePosition :: Float -> Asteroid -> Asteroid
updatePosition f a@(Asteroid _ _ p v _ _) =
  let (w, h) = Screen.dimensions in
  let (fw, fh) = ((fromIntegral w), (fromIntegral h)) in
  let (hw, hh) = (fw / 2, fh / 2) in
  let (Vector2 x y) = p + v in
  let nx = if x < (-hw) then x + fw else if x > hw then x - fw else x in
  let ny = if y < (-hh) then y + fh else if y > hh then y - fh else y in
  a {pos = Vector2 nx ny }

updateAsteroid :: Float -> Asteroid -> Asteroid
updateAsteroid f = (updateRotation f) . (updatePosition f)

scorePerHit :: Int
scorePerHit = 50

update :: Int -> Int -> Float -> Asteroids -> Bullets -> (Asteroids, Bullets, Int)
update n score f as bs = (new_asteroids, new_bullets, new_score) where
  collisions        = [(a, b) | a <- as, b <- bs, distance (bulletPos b) (pos a) < size a]
  dead_bullets      = map snd collisions
  hit_asteroids     = map fst collisions
  through_asteroids = filter (\a -> not (a `elem` hit_asteroids)) as
  split_asteroids   = concatMap (splitAsteroid n) hit_asteroids
  all_asteroids     = concat [split_asteroids, through_asteroids]
  new_asteroids     = filter alive $ map (updateAsteroid f) all_asteroids
  new_bullets       = filter (\b -> not (b `elem` dead_bullets)) bs
  new_score         = score + scorePerHit * length dead_bullets
