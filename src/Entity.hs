module Entity (
  Entity,
  new,
  render,
  handleEvent,
  update
) where

import Graphics.Gloss.Interface.Pure.Game

class Entity a where
  new         :: a
  render      :: a -> Picture
  handleEvent :: Event -> a -> a
  update      :: Float -> a -> a
