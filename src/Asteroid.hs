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

-- | An Asteroid. This is what the player will be shooting at.
data Asteroid =
  -- | Constructs a new Asteroid.
  Asteroid { angle :: Float,          -- ^ The angle of spin, in radians.
             size  :: Float,          -- ^ The maximum size, in pixels.
             pos   :: Vector2,        -- ^ The current position.
             vel   :: Vector2,        -- ^ The current velocity.
             shape :: [Vector2]       -- ^ The shape, as a list of
                                      -- concurrent vectors from the
                                      -- center.
           } deriving(Show, Eq)

-- | This is used to describe multiple Asteroids.
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

-- | Given an Asteroid, will return its position.
asteroidPosition :: Asteroid          -- ^ The asteroid to calculate
                                      -- the position of
                 -> Vector2           -- ^ The position of the
                                      -- asteroid
asteroidPosition = pos

-- | Generates a randomly determined set of asteroids given a random
-- seed.
generateInitial :: Int                -- ^ The PRNG seed used to
                                      -- generate the asteroids
                -> Asteroids          -- ^ The generated asteroids
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

-- | Given a set of Asteroids, will generate the Picture that
-- represetnts them.
render :: Asteroids                   -- ^ The Asteroids to compute a
                                      -- Picture of.
       -> Picture                     -- ^ The computed Picture.
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

-- | Updates the set of Asteroids. This method is where a lot of the
-- action is, as it calculates scores and destroys asteroids and
-- bullets.
update :: Int                         -- ^ The current frame. This is
                                      -- normally used as a PRNG seed
                                      -- when needed.
       -> Int                         -- ^ The current score. Any
                                      -- scored points will be added
                                      -- to this.
       -> Float                       -- ^ The time delta, in seconds,
                                      -- since the last time this
                                      -- function ran.
       -> Asteroids                   -- ^ The current set of
                                      -- Asteroids.
       -> Bullets                     -- ^ The current set of bullets.
       -> (Asteroids, Bullets, Int)   -- ^ The new set of Asteroids,
                                      -- Bullets, and the new score.
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
