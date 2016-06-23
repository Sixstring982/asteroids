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

data Store = Store Int Ship Bullets Asteroids

new :: Store
new = Store 0 Ship.new [] Asteroid.generateInitial

render :: Store -> Picture
render (Store _ ship bs as) = Pictures [Ship.render ship, Bullet.render bs, Asteroid.render as]

handleEvent :: Event -> Store -> Store
handleEvent e (Store n ship bs as) = Store n
                                           (Ship.handleEvent e ship)
                                           (Bullet.handleEvent e (noseHeading ship) bs)
                                           as

update :: Float -> Store -> Store
update f (Store n ship bs as) = Store (succ n) (Ship.update f as ship) new_bullets new_asteroids where
  moved_bullets = Bullet.update f bs
  (new_asteroids, new_bullets) = Asteroid.update f as moved_bullets
