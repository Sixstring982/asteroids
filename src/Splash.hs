module Splash (
  render
) where

import Graphics.Gloss

import Font
import Screen

fontSize :: Float
fontSize = 12

render :: Picture
render = color fgColor $ pictures [title, instrs] where
  title  = scale fontSize fontSize $ pictureFromString "asteroids"
  instrs = translate (-100) (-100) $
           scale (0.5 * fontSize) (0.5 * fontSize) $
           pictureFromString "press enter to play"
