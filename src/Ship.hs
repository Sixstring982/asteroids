module Ship (
  Ship(Ship, Exploded),
  Ship.new,
  Ship.render,
  Ship.handleEvent,
  Ship.update,
  noseHeadingVelocity
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Asteroid
import Entity
import Fragment
import Math
import Screen
import Vector2

data Acceleration = NoAcceleration | Forward | Backward
  deriving(Eq, Show)

-- | The rate that the ship will accelerate at, in pixels per second^2.
accelerationSpeed :: Float
accelerationSpeed = 6.0

-- | Given an acceleration type, time delta, vector, and heading,
-- applies an acceleration to the velocity vector.
applyAcceleration :: Acceleration     -- ^ The Acceleration type to apply
                  -> Float            -- ^ The time delta given by an update function
                  -> Vector2          -- ^ The current velocity
                  -> Vector2          -- ^ The current heading (normalized)
                  -> Vector2          -- ^ The resulting velocity
applyAcceleration NoAcceleration     _ v _ = v
applyAcceleration Forward            t v h = v + Vector2.scale (accelerationSpeed * t) h
applyAcceleration Backward           t v h = v - Vector2.scale (accelerationSpeed * t) h



data Rotation = NoRotation | LeftRotation | RightRotation
  deriving(Show)

-- | The rate that the ship will rotate at, in radians per second.
rotationSpeed :: Float
rotationSpeed = 3.0

-- | Given a rotation type, time delta, and angle, applies rotation to
-- the angle.
applyRotation :: Rotation             -- ^ The Rotation type to apply
              -> Float                -- ^ The time delta given by an update function
              -> Float                -- ^ The current angle
              -> Float                -- ^ The resulting angle
applyRotation NoRotation    _ a = a
applyRotation LeftRotation  f a = a + rotationSpeed * f
applyRotation RightRotation f a = a - rotationSpeed * f



data Ship = Ship { angle :: Float,
                   pos :: Vector2,
                   vel :: Vector2,
                   acceleration :: Acceleration,
                   rotation :: Rotation
                 }
          | Exploded Fragments
          deriving(Show)

noseHeadingVelocity :: Ship -> Maybe (Vector2, Vector2, Vector2)
noseHeadingVelocity (Exploded _) = Nothing
noseHeadingVelocity s = Just (nose, hding, vel s) where
  hding = heading s
  nose = (pos s) + (Vector2.scale shipSize hding)


-- | The size of the ship, Really, this is the number of pixels from
-- the nose to the center of the ship.
shipSize :: Float
shipSize = 20.0

-- | Calculates the heading of the ship as a normalized vector based
-- on its rotation angle.
heading :: Ship                       -- ^ The ship to calculate the heading of
        -> Vector2                    -- ^ The heading of the ship
heading (Ship a _ _ _ _) = fromPolar (a, 1)

shipTranslation :: Ship -> (Picture -> Picture)
shipTranslation (Ship a (Vector2 x y) _ _ _) = (translate x y) . (rotate (degFromRad (-a)))

shipPicture :: Ship -> Picture
shipPicture ship = Pictures [bodyPicture, flamePicture ship]

flameColor :: Color
flameColor = red

flamePicture :: Ship -> Picture
flamePicture (Ship _ _ _ a _) = if a /= Forward then Blank else flame where
  flame = toLineLoop $ (map (Vector2.scale shipSize)) $ (map fromPolar)
          [(0, 0), ((5.0 * pi) / 6.0, 0.5), (pi, 0.66), ((7.0 * pi) / 6.0, 0.5)]

bodyPicture :: Picture
bodyPicture =
  toLineLoop $ (map (Vector2.scale shipSize)) $ (map fromPolar)
  [(0, 1), ((2.0 * pi) / 3.0, 0.66), (0, 0), ((4.0 * pi) / 3.0, 0.66)]

render :: Ship -> Picture
render ship@(Ship _ _ _ _ _) = (shipTranslation ship) (shipPicture ship)
render (Exploded fs) = Fragment.render fs


new :: Ship
new = Ship { angle = 0.0,
             pos = zero,
             vel = zero,
             acceleration = NoAcceleration,
             rotation = NoRotation }

handleEvent :: Event -> Ship -> Ship
handleEvent _ s@(Exploded _) = s
handleEvent (EventKey (Char 'w') state _ _) s =
  s { acceleration = (if state == Up then NoAcceleration else Forward) }

handleEvent (EventKey (Char 's') state _ _) s =
  s { acceleration = (if state == Up then NoAcceleration else Backward) }

handleEvent (EventKey (Char 'a') state _ _) s =
  s { rotation = (if state == Up then NoRotation else LeftRotation) }

handleEvent (EventKey (Char 'd') state _ _) s =
  s { rotation = (if state == Up then NoRotation else RightRotation) }

handleEvent _ s = s

updateVelocity :: Float -> Ship -> Ship
updateVelocity f s@(Ship _ _ v a _) = s { vel = applyAcceleration a f v (heading s) }

wrapInBounds :: (Float, Float)
             -> (Float, Float)
             -> (Float, Float)
             -> (Float, Float)
wrapInBounds (min_x, min_y) (max_x, max_y) (x, y) = (new_x, new_y) where
  wrap v mn mx = if v < mn then mx + (mn - v)
                 else if v > mx then mn + (v - mx)
                      else v
  new_x = wrap x min_x max_x
  new_y = wrap y min_y max_y

wrapInScreen :: (Float, Float) -> (Float, Float)
wrapInScreen = wrapInBounds mins maxs where
  (w, h) = Screen.dimensions
  mins   = (fromIntegral (-(w `div` 2)), fromIntegral (-(h `div` 2)))
  maxs   = (fromIntegral (w `div` 2), fromIntegral (h `div` 2))

updatePosition :: Float -> Ship -> Ship
updatePosition f s@(Ship _ p v _ _) = s { pos = fromPoint $ wrapInScreen $ toPoint $ p + v }

updateAngle :: Float -> Ship -> Ship
updateAngle f s@(Ship a _ _ _ r) = s { angle = applyRotation r f a }

asteroidCollisionPadding :: Float
asteroidCollisionPadding = 0.5

updateLiveliness :: Int -> Asteroids -> Ship -> Ship
updateLiveliness n as s@(Ship _ p _ _ x) =
  let collisions = map (\ a -> let dist = distance (asteroidPosition a) p in
                               dist < shipSize + (size a * asteroidCollisionPadding)) as in
  let still_alive = not $ or collisions in
  if still_alive then s else Exploded (generateFragments n p)

update :: Int -> Float -> Asteroids -> Ship -> Ship
update n f _ (Exploded fs) = Exploded (Fragment.update f fs)
update n f as s = ((updateLiveliness n as) . (updateVelocity f) . (updatePosition f) . (updateAngle f)) s
