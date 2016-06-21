module Ship (
  Ship(Ship),
  new,
  render,
  handleEvent,
  update,
  noseHeading
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Entity
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
                   drawColor :: Color,
                   pos :: Vector2,
                   vel :: Vector2,
                   acceleration :: Acceleration,
                   rotation :: Rotation
                 } deriving(Show)

noseHeading :: Ship -> (Vector2, Vector2)
noseHeading s =
  let hding = heading s in
  let nose = (pos s) + (Vector2.scale shipSize hding) in
  (nose, hding)

-- | The size of the ship, Really, this is the number of pixels from
-- the nose to the center of the ship.
shipSize :: Float
shipSize = 20.0

-- | Calculates the heading of the ship as a normalized vector based
-- on its rotation angle.
heading :: Ship                       -- ^ The ship to calculate the heading of
        -> Vector2                    -- ^ The heading of the ship
heading (Ship a _ _ _ _ _) = fromPolar (a, 1)

shipTranslation :: Ship -> (Picture -> Picture)
shipTranslation (Ship a c (Vector2 x y) _ _ _) = (translate x y) . (rotate (degFromRad (-a))) . color c

shipPicture :: Ship -> Picture
shipPicture ship = pictures [bodyPicture, flamePicture ship]

flameColor :: Color
flameColor = red

flamePicture :: Ship -> Picture
flamePicture (Ship _ _ _ _ a _) =
  let flame = toLineLoop $ (map (Vector2.scale shipSize)) $ (map fromPolar)
        [(0, 0), ((5.0 * pi) / 6.0, 0.5), (pi, 0.66), ((7.0 * pi) / 6.0, 0.5)]
  in if a /= Forward then Blank else flame

bodyPicture :: Picture
bodyPicture =
  toLineLoop $ (map (Vector2.scale shipSize)) $ (map fromPolar)
  [(0, 1), ((2.0 * pi) / 3.0, 0.66), (0, 0), ((4.0 * pi) / 3.0, 0.66)]

_shipRender :: Ship -> Picture
_shipRender ship = (shipTranslation ship) (shipPicture ship)

_shipNew :: Ship
_shipNew = Ship { angle = 0.0,
                  drawColor = white,
                  pos = zero,
                  vel = zero,
                  acceleration = NoAcceleration,
                  rotation = NoRotation}

_shipHandleEvent :: Event -> Ship -> Ship
_shipHandleEvent (EventKey (Char 'w') state _ _) s =
  s { acceleration = (if state == Up then NoAcceleration else Forward) }

_shipHandleEvent (EventKey (Char 's') state _ _) s =
  s { acceleration = (if state == Up then NoAcceleration else Backward) }

_shipHandleEvent (EventKey (Char 'a') state _ _) s =
  s { rotation = (if state == Up then NoRotation else LeftRotation) }

_shipHandleEvent (EventKey (Char 'd') state _ _) s =
  s { rotation = (if state == Up then NoRotation else RightRotation) }

_shipHandleEvent _ s = s

updateVelocity :: Float -> Ship -> Ship
updateVelocity f s@(Ship _ _ _ v a _) = s { vel = applyAcceleration a f v (heading s) }

wrapInBounds :: (Float, Float)
             -> (Float, Float)
             -> (Float, Float)
             -> (Float, Float)
wrapInBounds (min_x, min_y) (max_x, max_y) (x, y) =
  let wrap v mn mx = if v < mn then mx + (mn - v)
                     else if v > mx then mn + (v - mx)
                          else v in
  let new_x = wrap x min_x max_x in
  let new_y = wrap y min_y max_y in
  (new_x, new_y)

wrapInScreen :: (Float, Float) -> (Float, Float)
wrapInScreen = wrapInBounds mins maxs where
  (w, h) = Screen.dimensions
  mins   = (fromIntegral (-(w `div` 2)), fromIntegral (-(h `div` 2)))
  maxs   = (fromIntegral (w `div` 2), fromIntegral (h `div` 2))

updatePosition :: Float -> Ship -> Ship
updatePosition f s@(Ship _ _ p v _ _) = s { pos = fromPoint $ wrapInScreen $ toPoint $ p + v }

updateAngle :: Float -> Ship -> Ship
updateAngle f s@(Ship a _ _ _ _ r) = s { angle = applyRotation r f a }

_shipUpdate :: Float -> Ship -> Ship
_shipUpdate f = (updateVelocity f) . (updatePosition f) . (updateAngle f)

instance Entity Ship where
  new         = _shipNew
  render      = _shipRender
  handleEvent = _shipHandleEvent
  update      = _shipUpdate
