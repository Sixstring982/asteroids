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
updateBulletRotation f (Bullet a p v x) = Bullet (a + f * rotateSpeed) p v x

updateBulletPosition :: Float -> Bullet -> Bullet
updateBulletPosition f (Bullet a p v x) = Bullet a (p + v) v x

updateBulletLiveliness :: Float -> Bullet -> Bullet
updateBulletLiveliness f (Bullet a p@(Vector2 x y) v _) =
  let (w, h) = Screen.dimensions in
  let (hw, hh) = (w `div` 2, h `div` 2) in
  let (ix, iy) = ((round x), (round y)) in
  let still_alive = ix > (-hw) && ix <= hw && iy > (-hh) && iy <= hh in
    Bullet a p v still_alive

updateBullet :: Float -> Bullet -> Bullet
updateBullet f = (updateBulletLiveliness f) . (updateBulletRotation f) . (updateBulletPosition f)

update :: Float -> Bullets -> Bullets
update f bs = filter alive $ map (updateBullet f) bs
