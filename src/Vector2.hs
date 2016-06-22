module Vector2 (
  Vector2(Vector2),
  zero,
  i,
  j,
  randomNormalized,
  Vector2.scale,
  magnitude,
  distance,
  normalize,
  fromPolar,
  toPoint,
  fromPoint,
  toLine,
  toLineLoop,
  toPolygon
) where

import System.Random
import Graphics.Gloss
import Math

-- | A vector with two components.
data Vector2
  -- | Constructs a Vector2 with its two components.
  = Vector2 Float Float
  deriving(Show, Eq)

instance Num Vector2 where
  (Vector2 ax ay) + (Vector2 bx by) = Vector2 (ax + bx) (ay + by)
  (Vector2 ax ay) * (Vector2 bx by) = Vector2 (ax * bx) (ay * by)
  (Vector2 ax ay) - (Vector2 bx by) = Vector2 (ax - bx) (ay - by)
  abs (Vector2 ax ay)               = Vector2 (abs ax) (abs ay)
  signum (Vector2 ax ay)            = Vector2 (signum ax) (signum ay)
  fromInteger i                     = Vector2 (fromIntegral i) 0

-- | A Vector2 with both of its components set to 0.0 .
zero :: Vector2
zero = Vector2 0 0

-- | A Vector2 of the basis vector I, which is a normalized vector in
-- the X direction.
i :: Vector2
i = Vector2 1 0

-- | A Vector2 of the basis vector J, which is a normalized vector in
-- the Y direction.
j :: Vector2
j = Vector2 0 1

-- | Generates a normalized Vector2 in a random direction.
randomNormalized :: RandomGen g => g  -- ^ The PRNG to generate
                                      --   coordinates with
                 -> (Vector2, g)      -- ^ The generated Vector2
randomNormalized g0 =
  (n, g2) where
  (x, g1) = randomR (-1.0, 1.0) g0
  (y, g2) = randomR (-1.0, 1.0) g1
  n       = normalize $ Vector2 x y


-- | Given a scalar and a Vector2, returns a version of the vector
-- with its components multiplied by the vector.
scale :: Float                        -- ^ The scalar to scale the vector by
      -> Vector2                      -- ^ The vector to scale
      -> Vector2                      -- ^ The scaled vector
scale f (Vector2 x y) = Vector2 (f * x) (f * y)

magnitude :: Vector2 -> Float
magnitude (Vector2 x y) = sqrt $ x * x + y * y

distance :: Vector2 -> Vector2 -> Float
distance a b = magnitude (a - b)

normalize :: Vector2 -> Vector2
normalize v@(Vector2 x y) = let mag = magnitude v in Vector2 (x / mag) (y / mag)

-- | Given a pair of polar coordinates, transforms then into a
-- Vector2 with euclidean components.
fromPolar :: (Float, Float)           -- ^ The polar coordinates (theta, r)
          -> Vector2                  -- ^ The euclidean vector
                                      -- representation of the polar
                                      -- coordinates
fromPolar (theta, r) = Vector2 (r * cos(theta)) (r * sin(theta))

-- | Given a Vector2, returns its components in a tuple.
toPoint :: Vector2                    -- ^ The Vector2 to turn into a Point
        -> Point                      -- ^ The coordinates of the Vector2
toPoint (Vector2 x y) = (x, y)

fromPoint :: Point -> Vector2
fromPoint (x, y) = Vector2 x y

-- | Given a list of Vector2 objects, turns them into a renderable
-- line.
toLine :: [Vector2]                   -- ^ The vectors to turn into a line
       -> Picture                     -- ^ The resulting line
toLine = toRenderable line

toLineLoop :: [Vector2]
           -> Picture
toLineLoop = toRenderable lineLoop


-- | Given a list of Vector2 objects, turns them into a renderable
-- polygon.
toPolygon :: [Vector2]                   -- ^ The vectors to turn into a polygon
          -> Picture                     -- ^ The resulting polygon
toPolygon = toRenderable polygon

toRenderable :: ([Point] -> Picture) -> [Vector2] -> Picture
toRenderable f = f . (map toPoint)
