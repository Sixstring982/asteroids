module Store (
  Store(Store),
  Store.new,
  Store.render,
  Store.handleEvent,
  Store.update
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Entity
import Ship

data Store = Store Ship

new :: Store
new = Store Ship.new

render :: Store -> Picture
render (Store ship) = Ship.render ship


handleEvent :: Event -> Store -> Store
handleEvent e (Store ship) = Store (Ship.handleEvent e ship)

update :: Float -> Store -> Store
update f (Store ship) = (Store (Ship.update f ship))
