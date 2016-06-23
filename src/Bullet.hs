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

-- | A Bullet. This is what the player fires.
data Bullet =
  -- | Constructs a Bullet.
  Bullet { angle :: Float,            -- ^ The rotation angle of the
                                      -- bullet. This is cosmetic.
           pos :: Vector2,            -- ^ The position of the bullet.
           vel :: Vector2,            -- ^ The velocity of the bullet.
           alive :: Bool              -- ^ Whether or not the bullet
                                      -- should matter anymore. If
                                      -- false, will be destroyed on
                                      -- update.
         } deriving(Eq, Show)

-- | This is used to represent a set of Bullets.
type Bullets = [Bullet]

-- | Given a Bullet, will compute its position.
bulletPos :: Bullet                   -- ^ The bullet to find the
                                      -- position of.
          -> Vector2                  -- ^ The bullet's position.
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

-- | Given a set of Bullets, will compute a Picture representing them.
render :: Bullets                     -- ^ The Bullets to compute a
                                      -- Picture of.
       -> Picture                     -- ^ The computed Picture.
render bs = Pictures $ map pictureFromBullet bs

-- | Given an Event (e.g. representing a User's key press) and some
-- other information, computes a new set of Bullets.
handleEvent :: Event                  -- ^ The Event to handle.
            -> Maybe (Vector2, Vector2, Vector2) -- ^ A tuple
                                                 -- representing the
                                                 -- position of the
                                                 -- Ship's nose, the
                                                 -- Ship's heading,
                                                 -- and the Ship's
                                                 -- velocity. If there
                                                 -- is no ship, this
                                                 -- should be Nothing.
            -> Bullets                -- ^ The current set of Bullets.
            -> Bullets                -- ^ The new set of Bullets.
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

-- | Given a time delta, determines how the Bullets will act over the
-- given time span.
update :: Float                       -- ^ A time delta, in seconds
                                      -- since the last time this
                                      -- function was called.
       -> Bullets                     -- ^ The set of bullets to
                                      -- update.
       -> Bullets                     -- ^ The updated set of Bullets.
update f bs = filter alive $ map (updateBullet f) bs
