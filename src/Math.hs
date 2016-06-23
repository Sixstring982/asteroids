module Math (
  stot,
  degFromRad,
  digits
) where

stot :: Float
stot = (sqrt 2) / 2

degFromRad :: Float -> Float
degFromRad a = (a / pi) * 180.0

digits :: Int -> [Int]
digits n =
  if n == 0 then [0]
  else let loop n acc =
             if n == 0 then acc
             else loop (n `div` 10) ((n `mod` 10) : acc)
       in loop n []
