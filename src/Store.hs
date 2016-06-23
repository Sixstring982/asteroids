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
import Splash
import Ship

data Store =
  Store Int Int Ship Bullets Asteroids
  | SplashScreen Int Int

new :: Store
new = SplashScreen 0 0

withFrame :: Int -> Store
withFrame n = Store n 0 Ship.new [] (Asteroid.generateInitial n)

scoreSize :: Float
scoreSize = 12

renderScore :: Int -> Picture
renderScore n = translate x y $ scale scoreSize scoreSize $ pictureFromInt n where
  (w, h) = Screen.dimensions
  (hw, hh) = ((fromIntegral w) / 2, (fromIntegral h) / 2)
  (mx, my) = (20, 20)
  (x, y) = (mx - hw, hh - my)

render :: Store -> Picture
render (Store _ score ship bs as) = color Screen.fgColor $
  Pictures [renderScore score, Ship.render ship, Bullet.render bs, Asteroid.render as]
render (SplashScreen _ score) = Splash.render score

handleEvent :: Event -> Store -> Store
handleEvent e (Store n s ship bs as) = Store n s
                                           (Ship.handleEvent e ship)
                                           (Bullet.handleEvent e (noseHeading ship) bs)
                                           as
handleEvent (EventKey (SpecialKey KeyEnter) Down (Modifiers Up Up Up)  _) (SplashScreen n _) = Store.withFrame n
handleEvent _ s = s

update :: Float -> Store -> Store
update f (SplashScreen n score) = SplashScreen (succ n) score
update f (Store n score (Exploded []) _ _) = SplashScreen n score
update f (Store n score ship bs as) = Store (succ n)
                                            new_score
                                            (Ship.update n f as ship) new_bullets new_asteroids where
  moved_bullets = Bullet.update f bs
  (new_asteroids, new_bullets, new_score) = Asteroid.update n score f as moved_bullets
