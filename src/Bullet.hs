module Bullet (
  Bullet(Bullet),
  Bullets,
  bulletPos,
  render,
  handleEvent,
  update
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Math
import Screen
import Vector2

data Bullet = Bullet { angle :: Float,
                       pos :: Vector2,
                       vel :: Vector2,
                       alive :: Bool
                     } deriving(Eq, Show)

type Bullets = [Bullet]

bulletPos :: Bullet -> Vector2
bulletPos = pos

fromNoseHeadingVelocity :: (Vector2, Vector2, Vector2) -> Bullet
fromNoseHeadingVelocity (p, h, v) = Bullet 0 p (v + h)  True

bulletSize :: Float
bulletSize = 2.0

bulletSpeed :: Float
bulletSpeed = 1.0

rotateSpeed :: Float
rotateSpeed = 10.0

bulletPicture :: Picture
bulletPicture = line [(-2, 0), (2, 0)]

pictureFromBullet :: Bullet -> Picture
pictureFromBullet (Bullet a (Vector2 x y) _ _) =
  translate x y $ rotate (degFromRad a) $ bulletPicture

render :: Bullets -> Picture
render bs = Pictures $ map pictureFromBullet bs

handleEvent :: Event -> Maybe (Vector2, Vector2, Vector2) -> Bullets -> Bullets
handleEvent (EventKey (Char 'e') state _ _) (Just nhv) bs =
  if state == Up then bs
  else (fromNoseHeadingVelocity nhv) : bs
handleEvent _ _ bs = bs

updateBulletRotation :: Float -> Bullet -> Bullet
updateBulletRotation f b@(Bullet a _ _ _) = b { angle = a + f * rotateSpeed }

updateBulletPosition :: Float -> Bullet -> Bullet
updateBulletPosition _ b@(Bullet _ p v _) = b { pos = (p + v) }

updateBulletLiveliness :: Float -> Bullet -> Bullet
updateBulletLiveliness f b@(Bullet _ (Vector2 x y) _ _) = b { alive = still_alive } where
  (w, h) = Screen.dimensions
  (hw, hh) = (w `div` 2, h `div` 2)
  (ix, iy) = ((round x), (round y))
  still_alive = ix > (-hw) && ix <= hw && iy > (-hh) && iy <= hh

updateBullet :: Float -> Bullet -> Bullet
updateBullet f = (updateBulletLiveliness f) . (updateBulletRotation f) . (updateBulletPosition f)

update :: Float -> Bullets -> Bullets
update f bs = filter alive $ map (updateBullet f) bs
