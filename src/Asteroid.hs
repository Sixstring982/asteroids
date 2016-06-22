module Asteroid (
  Asteroid(Asteroid),
  Asteroids,
  generateInitial,
  update,
  render,
) where

import Graphics.Gloss
import Math
import Vector2

data Asteroid = Asteroid Float Vector2 Vector2 [Vector2]

type Asteroids = [Asteroid]

rotationRate :: Float
rotationRate = 3.0

initialAsteroidCount :: Int
initialAsteroidCount = 10

generateInitial :: Asteroids
generateInitial = [generateAsteroid | a <- [1..initialAsteroidCount]]

generateAsteroid :: Asteroid
generateAsteroid =
  let angles = [((fromIntegral a) * pi) / 4.0 | a <- [0..8]] in
    Asteroid 0.0 Vector2.zero Vector2.zero
    [fromPolar (a, 30.0) | a <- angles]

pictureFromAsteroid :: Asteroid -> Picture
pictureFromAsteroid (Asteroid a (Vector2 x y) _ vs) =
  color white $ translate x y $ rotate (degFromRad a) $ toLineLoop vs

render :: Asteroids -> Picture
render as = Pictures [pictureFromAsteroid a | a <- as]

updateRotation :: Float -> Asteroid -> Asteroid
updateRotation f (Asteroid a p v vs) = Asteroid (a + f * rotationRate) p v vs

updateAsteroid :: Float -> Asteroid -> Asteroid
updateAsteroid f = updateRotation f

update :: Float -> Asteroids -> Asteroids
update f = map (updateAsteroid f)
