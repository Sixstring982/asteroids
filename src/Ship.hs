module Ship (
  Ship(Ship),
  new,
  render,
  handleEvent,
  update
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Entity
import Vector2

data Acceleration = None | Forward | Backward
  deriving(Show)

accelerationSpeed :: Float
accelerationSpeed = 20.0

-- | Given a time delta, vector, and heading, applies an acceleration to the
-- velocity vector.
applyAcceleration :: Acceleration     -- ^ The Acceleration type to apply
                  -> Float            -- ^ The time delta given by an update function
                  -> Vector2          -- ^ The current velocity
                  -> Vector2          -- ^ The current heading (normalized)
                  -> Vector2          -- ^ The resulting velocity
applyAcceleration None     _ v _ = v
applyAcceleration Forward  t v h = v + Vector2.scale (accelerationSpeed * t) h
applyAcceleration Backward t v h = v - Vector2.scale (accelerationSpeed * t) h

data Ship = Ship { angle :: Float,
                   drawColor :: Color,
                   pos :: Vector2,
                   vel :: Vector2,
                   acceleration :: Acceleration
                 } deriving(Show)

shipSize :: Float
shipSize = 20.0

heading :: Ship -> Vector2
heading (Ship a _ _ _ _) = fromPolar (a, 1)

shipShape :: Picture
shipShape = let first = (0, 1) in
  toLine $ (map (Vector2.scale shipSize)) $ (map fromPolar)
  [first, ((2.0 * pi) / 3.0, 0.66), (0, 0), ((4.0 * pi) / 3.0, 0.66), first]

_shipRender :: Ship -> Picture
_shipRender (Ship a c (Vector2 x y) _ _) = color c $ rotate a $ translate x y $ shipShape

_shipNew :: Ship
_shipNew = Ship { angle = 0.0,
                  drawColor = white,
                  pos = zero,
                  vel = zero,
                  acceleration = None}

_shipHandleEvent :: Event -> Ship -> Ship
_shipHandleEvent (EventKey (Char 'w') state _ _) s = s { acceleration = (if state == Up then None else Forward) }
_shipHandleEvent (EventKey (Char 's') state _ _) s = s { acceleration = (if state == Up then None else Backward) }
_shipHandleEvent _ s = s

updateVelocity :: Float -> Ship -> Ship
updateVelocity f s@(Ship _ _ _ v a) = s { vel = applyAcceleration a f v (heading s) }

updatePosition :: Float -> Ship -> Ship
updatePosition f s@(Ship _ _ p v _) = s { pos = p + v }

_shipUpdate :: Float -> Ship -> Ship
_shipUpdate f = (updateVelocity f) . (updatePosition f)

instance Entity Ship where
  new         = _shipNew
  render      = _shipRender
  handleEvent = _shipHandleEvent
  update      = _shipUpdate
