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
  Store { frame :: Int,
          score :: Int,
          ship  :: Ship,
          bullets :: Bullets,
          asteroids :: Asteroids
        }
  | SplashScreen { splashFrame :: Int,
                   finalScore :: Int
                 } deriving(Show)

new :: Store
new = SplashScreen { splashFrame = 0, finalScore = 0 }

withFrame :: Int -> Store
withFrame n = Store {frame = n, score = 0, ship = Ship.new,
                     bullets = [], asteroids = (Asteroid.generateInitial n) }

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
handleEvent e s@(Store _ _ ship bs as) = s { ship = Ship.handleEvent e ship,
                                             bullets = Bullet.handleEvent e
                                                       (noseHeadingVelocity ship)
                                                       bs,
                                             asteroids = as }
handleEvent (EventKey (SpecialKey KeyEnter) Down (Modifiers Up Up Up)  _) (SplashScreen n _) = Store.withFrame n
handleEvent _ s = s

update :: Float -> Store -> Store
update f s@(SplashScreen n _) = s { splashFrame = (succ n) }
update f (Store n score (Exploded []) _ _) = SplashScreen n score
update f (Store n score ship bs as) = Store (succ n)
                                            new_score
                                            (Ship.update n f as ship) new_bullets new_asteroids where
  moved_bullets = Bullet.update f bs
  (new_asteroids, new_bullets, new_score) = Asteroid.update n score f as moved_bullets
