module Asteroid (
  Asteroid(Asteroid),
  Asteroids,
  size,
  asteroidPosition,
  generateInitial,
  update,
  render,
) where

import System.Random
import Graphics.Gloss
import Math
import Screen
import Vector2

data Asteroid = Asteroid { angle :: Float,
                           size :: Float,
                           pos :: Vector2,
                           vel :: Vector2,
                           shape :: [Vector2],
                           alive :: Bool
                         } deriving(Show)

type Asteroids = [Asteroid]

maxAsteroidSize :: Float
maxAsteroidSize = 30.0

maxVelocity :: Float
maxVelocity = 5.0

asteroidArmCount :: Int
asteroidArmCount = 8

rotationRate :: Float
rotationRate = 3.0

initialAsteroidCount :: Int
initialAsteroidCount = 10

asteroidSeed :: Int
asteroidSeed = 1024

asteroidPosition :: Asteroid -> Vector2
asteroidPosition = pos

generateInitial :: Asteroids
generateInitial =
  let rand = mkStdGen asteroidSeed in
  let (as, _) =
        foldl (\ (ls, g) n -> let (a, new_g) = generateAsteroid g in
                              (a : ls, new_g))
              ([], rand)
              [1..initialAsteroidCount]
  in as

generateRadius :: RandomGen g => g -> (Float, g)
generateRadius = randomR (0.5 * maxAsteroidSize, maxAsteroidSize)

generateArms :: RandomGen g => g -> ([Vector2], g)
generateArms g0 =
  foldl (\ (ls, g) n -> let (r, gn) = generateRadius g in
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

generateAsteroid :: RandomGen g => g -> (Asteroid, g)
generateAsteroid g0 =
  let (pos, g1)  = generatePosition g0 in
  let (vel, g2)  = generateVelocity g1 in
  let (arms, g3) = generateArms g2 in
  (Asteroid 0 maxAsteroidSize pos vel arms True, g3)

pictureFromAsteroid :: Asteroid -> Picture
pictureFromAsteroid (Asteroid a _ (Vector2 x y) _ vs _) =
  color white $ translate x y $ rotate (degFromRad a) $ toLineLoop vs

render :: Asteroids -> Picture
render as = Pictures [pictureFromAsteroid a | a <- as]

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

update :: Float -> Asteroids -> Asteroids
update f as = filter alive $ map (updateAsteroid f) as
