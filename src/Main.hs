module Main where

import qualified Astar
import qualified Mapgen
import Brick
import Graphics.Vty

ui :: Widget ()
ui = str "Hello, world!"

eventHandler :: s -> BrickEvent n e -> EventM n (Next s)
eventHandler state e = continue state

app :: App s e ()
app = App
  { appDraw = const [ui]
  , appChooseCursor = showFirstCursor
  , appStartEvent = return
  , appAttrMap = const $ attrMap (white `on` black) []
  , appHandleEvent = eventHandler
  }

main :: IO ()
main = simpleMain ui
-- main :: IO ()
-- main = Mapgen.main
