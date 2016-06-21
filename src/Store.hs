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
import Bullet
import Ship

data Store = Store Ship Bullets

new :: Store
new = Store Ship.new []

render :: Store -> Picture
render (Store ship bs) = Pictures [Ship.render ship, Bullet.render bs]

handleEvent :: Event -> Store -> Store
handleEvent e (Store ship bs) = Store (Ship.handleEvent e ship) (Bullet.handleEvent e (noseHeading ship) bs)

update :: Float -> Store -> Store
update f (Store ship bs) = Store (Ship.update f ship) (Bullet.update f bs)
