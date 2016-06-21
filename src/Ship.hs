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

data Ship = Ship { angle :: Float,
                   drawColor :: Color,
                   pos :: Vector2,
                   vel :: Vector2
                 } deriving(Show)

shipSize :: Float
shipSize = 20.0

shipShape :: Picture
shipShape = let first = (0, 1) in
  toLine $ (map (Vector2.scale shipSize)) $ (map fromPolar)
  [first, ((2.0 * pi) / 3.0, 0.66), (0, 0), ((4.0 * pi) / 3.0, 0.66), first]

_shipRender :: Ship -> Picture
_shipRender (Ship a c (Vector2 x y) _) = color c $ rotate a $ translate x y $ shipShape

_shipNew :: Ship
_shipNew = Ship { angle = 0.0,
                  drawColor = white,
                  pos = zero,
                  vel = zero}

_shipHandleEvent :: Event -> Ship -> Ship
_shipHandleEvent _ s = s

_shipUpdate :: Float -> Ship -> Ship
_shipUpdate f (Ship a c p v) = Ship a c (p + v) v

instance Entity Ship where
  new         = _shipNew
  render      = _shipRender
  handleEvent = _shipHandleEvent
  update      = _shipUpdate
