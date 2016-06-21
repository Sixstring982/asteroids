module Store (
  Store(Store),
  new,
  render,
  handleEvent,
  update
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Store = Store Float

new :: Store
new = Store 0.0

render :: Store -> Picture
render (Store f) = color white $ rotate f $ line [(100, -100), (0, 200), (-100, -100), (100, -100)]


handleEvent :: Event -> Store -> Store
handleEvent _ s = s

update :: Float -> Store -> Store
update f (Store a) = (Store (20.0 * f + a))
