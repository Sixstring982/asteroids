module Fragment (
  Fragment(Fragment),
  Fragments,
  generateFragments,
  render,
  update
) where

import System.Random
import Graphics.Gloss
import Math
import Screen
import Vector2

data Fragment = Fragment { angle :: Float,
                           rotationRate :: Float,
                           length :: Float,
                           pos :: Vector2,
                           vel :: Vector2,
                           alive :: Bool
                         } deriving(Show)

type Fragments = [Fragment]

maxRotationRate :: Float
maxRotationRate = 10.0

maxFragmentLength :: Float
maxFragmentLength = 10.0

maxFragmentSpeed :: Float
maxFragmentSpeed = 5.0

generatedFragmentCount :: Int
generatedFragmentCount = 20

generateFragment :: RandomGen g => g -> Vector2 -> (Fragment, g)
generateFragment g0 p = (Fragment a rate len p (Vector2.scale speed hding) True, g5) where
  (a, g1)     = randomR (0, pi) g0
  (rate, g2)  = randomR (0.1, maxRotationRate) g1
  (len, g3)   = randomR (0.1 * maxFragmentLength, maxFragmentLength) g2
  (speed, g4) = randomR (0.1 * maxFragmentSpeed, maxFragmentSpeed) g3
  (hding, g5) = randomNormalized g4

-- | Given a random seed and a position, generates a random list of
-- Framents which fly outwards from the position.
generateFragments :: Int              -- ^ The seed to initialize the
                                      -- PRNG with
                  -> Vector2          -- ^ The position that the
                                      -- Fragments should disperse
                                      -- from
                  -> Fragments        -- ^ The generate Fragments.
generateFragments n p = fs where
  (fs, _) =
    foldr (\ _ (fs, g0) -> let (f, g1) = generateFragment g0 p in
                           (f : fs, g1))
          ([], mkStdGen n)
          [1..generatedFragmentCount]

fragmentPicture :: Fragment -> Picture
fragmentPicture (Fragment _ _ l _ _ _) = line [(-l, 0), (l, 0)]

renderFragment :: Fragment -> Picture
renderFragment f@(Fragment a _ _ (Vector2 x y) _ _) =
  translate x y $ rotate (degFromRad a) $ fragmentPicture f

render :: Fragments -> Picture
render fs = Pictures $ map renderFragment fs

updateFragmentPosition :: Float -> Fragment -> Fragment
updateFragmentPosition f fr@(Fragment _ _ _ p v _) = fr { pos = p + v }

updateFragmentAngle :: Float -> Fragment -> Fragment
updateFragmentAngle f fr@(Fragment a r _ _ _ _) = fr { angle = a + f * r }

updateLiveliness :: Fragment -> Fragment
updateLiveliness f@(Fragment _ _ _ (Vector2 x y) _ _) = f { alive = still_alive } where
  (w, h)   = Screen.dimensions
  (hw, hh) = ((fromIntegral w) / 2, (fromIntegral h) / 2)
  still_alive = x > (-hw) && x < hw && y > (-hh) && y < hh

updateFragment :: Float -> Fragment -> Fragment
updateFragment f = updateLiveliness . (updateFragmentAngle f) . (updateFragmentPosition f)

update :: Float -> Fragments -> Fragments
update f fs = filter alive $ map (updateFragment f) fs
