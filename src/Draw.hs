{-# LANGUAGE OverloadedStrings #-}
module Draw (draw) where

import qualified Brick.Widgets.Core as W
import Brick
import Brick.Types
import Types
import Constants
import Control.Lens

makeWidgetFromStr :: String -> Widget ()
makeWidgetFromStr = padRight (Pad 1) . W.str

draw :: GameState -> [Widget ()]
draw state = let
    emptyBoard = replicate boardHeight (replicate boardWidth (makeWidgetFromStr " "))
    withWalls = foldl (\acc (Point x y) -> set (element y . element x) (makeWidgetFromStr wallCharacter) acc) emptyBoard (_walls state)
    withEnemies = foldl (\acc (Point x y) -> set (element y . element x) (withAttr "enemy" $ makeWidgetFromStr enemyCharacter) acc) withWalls (map _pos $ _enemies state)
    Point playerX playerY = view (player . pos) state
    withPlayer = set (element playerY . element playerX) (withAttr "player" $ makeWidgetFromStr playerCharacter) withEnemies
  in [W.vBox (map W.hBox withPlayer)]
