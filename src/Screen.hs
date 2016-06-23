module Screen (
  dimensions,
  position,
  fps,
  bgColor,
  fgColor
) where

import Graphics.Gloss

dimensions :: (Int, Int)
dimensions = (640, 480)

position :: (Int, Int)
position = (10, 10)

fps :: Int
fps = 60

bgColor :: Color
bgColor = black

fgColor :: Color
fgColor = white
