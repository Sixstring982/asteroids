module Vector2 (
  Vector2(Vector2),
  zero,
  i,
  j,
  Vector2.scale,
  fromPolar,
  toPoint,
  toLine,
  toPolygon
) where

import Graphics.Gloss

data Vector2 = Vector2 Float Float
  deriving(Show)

instance Num Vector2 where
  (Vector2 ax ay) + (Vector2 bx by) = Vector2 (ax + bx) (ay + by)
  (Vector2 ax ay) * (Vector2 bx by) = Vector2 (ax * bx) (ay * by)
  (Vector2 ax ay) - (Vector2 bx by) = Vector2 (ax - bx) (ay - by)
  abs (Vector2 ax ay)               = Vector2 (abs ax) (abs ay)
  signum (Vector2 ax ay)            = Vector2 (signum ax) (signum ay)
  fromInteger i                     = Vector2 (fromIntegral i) 0

zero :: Vector2
zero = Vector2 0 0

scale :: Float -> Vector2 -> Vector2
scale f (Vector2 x y) = Vector2 (f * x) (f * y)

i :: Vector2
i = Vector2 1 0

j :: Vector2
j = Vector2 0 1

fromPolar :: (Float, Float) -> Vector2
fromPolar (theta, r) = Vector2 (r * cos(theta)) (r * sin(theta))

toPoint :: Vector2 -> Point
toPoint (Vector2 x y) = (x, y)

toRenderable :: ([Point] -> Picture) -> [Vector2] -> Picture
toRenderable f = f . (map toPoint)

toLine :: [Vector2] -> Picture
toLine = toRenderable line

toPolygon :: [Vector2] -> Picture
toPolygon = toRenderable polygon
