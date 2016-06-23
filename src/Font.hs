module Font (
  pictureFromInt,
  pictureFromDigit
) where

import Graphics.Gloss
import Math
import Vector2

coordFromIndex :: Int -> Vector2
coordFromIndex n = Vector2 x y where
  x = case n `mod` 3 of
        0 -> stot
        1 -> -stot
        _ -> 0
  y = case (n - 1) `div` 3 of
        0 -> stot
        1 -> 0
        _ -> -stot

pictureFromCoords :: [Int] -> Picture
pictureFromCoords = toLine . map coordFromIndex

coords :: Int -> [Int]
coords n = case n of
             0 -> [7, 1, 3, 9, 7, 3]
             1 -> [4, 2, 8, 7, 9]
             2 -> [1, 3, 6, 7, 9]
             3 -> [1, 3, 6, 4, 6, 9, 7]
             4 -> [9, 3, 4, 6]
             5 -> [3, 1, 4, 6, 9, 7]
             6 -> [1, 7, 9, 6, 4]
             7 -> [1, 3, 9]
             8 -> [1, 3, 9, 7, 1, 4, 6]
             9 -> [7, 9, 3, 1, 4, 6]

pictureFromDigit :: Int -> Picture
pictureFromDigit = pictureFromCoords . coords

picturesFromInt :: Int -> [Picture]
picturesFromInt = map (pictureFromCoords . coords) . digits

pictureFromInt :: Int -> Picture
pictureFromInt n = pictures $
  foldl (\acc (xc, p) -> ((translate (3 * stot * (fromIntegral xc)) 0) p) : acc)
        []
        (zip [0..] (picturesFromInt n))
