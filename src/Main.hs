module Main where

import Control.Monad (void)
import qualified Astar
import Mapgen (genWalls)
import Brick
import qualified Brick.Widgets.Core as W
import Graphics.Vty
import Constants
import Types
import Control.Lens


draw :: GameState -> [Widget ()]
draw state = let
    emptyBoard = replicate boardHeight (replicate boardWidth W.emptyWidget)
    withWalls = foldl (\acc (Point x y) -> set (element y . element x) (W.str wallCharacter) acc) emptyBoard (walls state)
    withEnemies = foldl (\acc (Point x y) -> set (element y . element x) (W.str enemyCharacter) acc) withWalls (map pos $ enemies state)
    Point playerX playerY = pos $ player state
    withPlayer = set (element playerY . element playerX) (W.str playerCharacter) withEnemies
  in [W.hBox $ map W.vBox withPlayer]

eventHandler :: GameState -> BrickEvent n appEvent -> EventM n (Next GameState)
eventHandler state brickEvent = case brickEvent of
  VtyEvent vtyEvent -> case vtyEvent of
    EvKey evKey [] -> case evKey of
      KEsc -> halt state
      KChar 'q' -> halt state
      KUp -> continue state
      KDown -> continue state
      KLeft -> continue state
      KRight -> continue state
    _ -> continue state
  _ -> continue state

app :: App GameState appEvent ()
app = App
  { appDraw = draw
  , appChooseCursor = showFirstCursor
  , appStartEvent = return
  , appAttrMap = const $ attrMap (white `on` black) []
  , appHandleEvent = eventHandler
  }

initialState :: IO GameState
initialState = do
  w <- genWalls
  return $ GameState {player = defaultPlayer, enemies = [], walls = w}

main :: IO ()
main = void $ initialState >>= defaultMain app
