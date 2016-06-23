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
import Font
import Asteroid
import Bullet
import Screen
import Ship

data Store = Store Int Int Ship Bullets Asteroids

new :: Store
new = Store 0 0 Ship.new [] Asteroid.generateInitial

scoreSize :: Float
scoreSize = 20

renderScore :: Int -> Picture
renderScore n = translate x y $ scale scoreSize scoreSize $ pictureFromDigit n where
  (w, h) = Screen.dimensions
  (hw, hh) = ((fromIntegral w) / 2, (fromIntegral h) / 2)
  (mx, my) = (20, 20)
  (x, y) = (mx - hw, hh - my)


render :: Store -> Picture
render (Store _ score ship bs as) = color Screen.fgColor $
  Pictures [renderScore score, Ship.render ship, Bullet.render bs, Asteroid.render as]

handleEvent :: Event -> Store -> Store
handleEvent e (Store n s ship bs as) = Store n s
                                           (Ship.handleEvent e ship)
                                           (Bullet.handleEvent e (noseHeading ship) bs)
                                           as

update :: Float -> Store -> Store
update f (Store n score ship bs as) = Store (succ n)
                                            score
                                            (Ship.update n f as ship) new_bullets new_asteroids where
  moved_bullets = Bullet.update f bs
  (new_asteroids, new_bullets) = Asteroid.update n f as moved_bullets
