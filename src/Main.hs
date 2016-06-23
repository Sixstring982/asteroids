module Main (
  main
) where

import Graphics.Gloss
import Screen
import Store

window :: Display
window = InWindow "Asteroids" Screen.dimensions Screen.position

main :: IO ()
main = play window Screen.bgColor Screen.fps Store.new Store.render Store.handleEvent Store.update
