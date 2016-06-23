module Font (
  pictureFromInt,
  pictureFromString
) where

import Data.Char
import Graphics.Gloss
import Math
import Vector2

class Picturable a where
  toPicture :: a -> Picture

instance Picturable Int where
  toPicture = pictureFromCoords . coords

instance Picturable Char where
  toPicture = pictureFromCoords . charCoords

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
coords 0 = [7, 1, 3, 9, 7, 3]
coords 1 = [4, 2, 8, 7, 9]
coords 2 = [1, 3, 6, 7, 9]
coords 3 = [1, 3, 6, 4, 6, 9, 7]
coords 4 = [9, 3, 4, 6]
coords 5 = [3, 1, 4, 6, 9, 7]
coords 6 = [1, 7, 9, 6, 4]
coords 7 = [1, 3, 9]
coords 8 = [1, 3, 9, 7, 1, 4, 6]
coords 9 = [7, 9, 3, 1, 4, 6]

charCoords :: Char -> [Int]
charCoords 'A' = [7, 4, 2, 6, 4, 6, 9]
charCoords 'B' = [7, 1, 2, 6, 4, 5, 9, 7]
charCoords 'C' = [3, 1, 7, 9]
charCoords 'D' = [7, 1, 2, 6, 9, 7]
charCoords 'E' = [3, 1, 4, 6, 4, 7, 9]
charCoords 'F' = [3, 1, 7, 4, 6]
charCoords 'G' = [3, 1, 7, 9, 6, 5]
charCoords 'H' = [1, 7, 4, 6, 9, 3]
charCoords 'I' = [1, 3, 2, 8, 7, 9]
charCoords 'J' = [2, 3, 6, 8, 4]
charCoords 'K' = [1, 7, 4, 2, 4, 9]
charCoords 'L' = [1, 7, 9]
charCoords 'M' = [7, 1, 5, 3, 9]
charCoords 'N' = [7, 1, 9, 3]
charCoords 'O' = [1, 7, 9, 3, 1]
charCoords 'P' = [7, 1, 3, 6, 4]
charCoords 'Q' = [9, 3, 1, 7, 9, 5]
charCoords 'R' = [7, 1, 3, 6, 4, 9]
charCoords 'S' = [3, 1, 4, 6, 9, 7]
charCoords 'T' = [1, 3, 2, 8]
charCoords 'U' = [1, 7, 9, 3]
charCoords 'V' = [1, 4, 8, 6, 3]
charCoords 'W' = [1, 7, 5, 9, 3]
charCoords 'X' = [1, 9, 5, 7, 3]
charCoords 'Y' = [1, 5, 3, 5, 8]
charCoords 'Z' = [1, 3, 7, 9]

pictureFromPicturables :: Picturable a => [a] -> Picture
pictureFromPicturables ps = pictures $
  foldl (\acc (xc, p) -> ((translate (3 * stot * (fromIntegral xc)) 0) p) : acc)
        []
        (zip [0..] (map toPicture ps))

pictureFromInt :: Int -> Picture
pictureFromInt = pictureFromPicturables . digits

pictureFromString :: String -> Picture
pictureFromString = pictureFromPicturables . (map toUpper)
