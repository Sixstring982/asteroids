module Main (
  main
) where

import Graphics.Gloss
import Screen
import Store

window :: Display
window = InWindow "Asteroids" Screen.dimensions Screen.position

bgColor :: Color
bgColor = black

main :: IO ()
main = play window bgColor Screen.fps Store.new Store.render Store.handleEvent Store.update
