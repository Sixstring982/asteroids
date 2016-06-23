module Splash (
  render
) where

import Graphics.Gloss

import Font
import Screen

fontSize :: Float
fontSize = 12

render :: Int -> Picture
render score = color fgColor $ pictures [scoreDisplay, title, instrs] where
  title  = scale fontSize fontSize $ pictureFromString "asteroids"
  instrs = translate (-100) (-100) $
           scale (0.5 * fontSize) (0.5 * fontSize) $
           pictureFromString "press enter to play"
  scoreDisplay = if score == 0 then Blank
                 else translate (-100) 100 $
                      scale (0.5 * fontSize) (0.5 * fontSize) $
                      pictureFromInt score
