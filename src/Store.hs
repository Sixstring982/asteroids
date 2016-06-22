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
import Asteroid
import Bullet
import Ship

data Store = Store Ship Bullets Asteroids

new :: Store
new = Store Ship.new [] Asteroid.generateInitial

render :: Store -> Picture
render (Store ship bs as) = Pictures [Ship.render ship, Bullet.render bs, Asteroid.render as]

handleEvent :: Event -> Store -> Store
handleEvent e (Store ship bs as) = Store (Ship.handleEvent e ship) (Bullet.handleEvent e (noseHeading ship) bs) as

update :: Float -> Store -> Store
update f (Store ship bs as) = Store (Ship.update f as ship)
                                    (Bullet.update f bs)
                                    (Asteroid.update f as)
