{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Astar
import           Brick
import           Constants
import           Control.Lens
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Draw
import           Graphics.Vty
import           Mapgen                 (genWalls)
import           System.Random.Shuffle  (shuffleM)
import           Types

pointToCoord

nextPlace :: GameState -> Point -> Point
nextPlace state start =
  let
    waysFun = Astar.possibleWays $ getUnavailiblePoints state
    (x, y) = head $ Astar.flood (view (player . pos) state) start waysFun Astar.cost
  in Point x y

eventHandler :: GameState -> BrickEvent n appEvent -> EventM n (Next GameState)
eventHandler state brickEvent = case brickEvent of
  VtyEvent vtyEvent -> case vtyEvent of
    EvKey evKey [] -> case evKey of
      KEsc      -> halt state
      KChar 'q' -> halt state
      KChar 'r' -> liftIO (makeState (length $ _enemies state)) >>= continue
      KUp       -> continue state
      KDown     -> continue state
      KLeft     -> continue state
      KRight    -> continue state
    _ -> continue state
  _ -> continue state

getShuffledAvailablePoints :: [Point] -> IO [Point]
getShuffledAvailablePoints unavailablePoints = shuffleM [Point x y |
                                                         x <- [0..(boardWidth-1)],
                                                         y <- [0..(boardHeight-1)],
                                                         Point x y `notElem` unavailablePoints]
getUnavailiblePoints :: GameState -> [Point]
getUnavailiblePoints state =
  let movables = _player state : _enemies state
  in _walls state ++ map (view pos) movables

makeState :: Int -> IO GameState
makeState prevNumEnemies = do
  walls <- genWalls
  availiblePositions <- getShuffledAvailablePoints walls
  let (enemyPositions, availiblePositionsAfterEnemies) = splitAt (prevNumEnemies+1) availiblePositions
  let ([playerPosition], availiblePositionsAfterPlayer) = splitAt 1 availiblePositionsAfterEnemies
  let enemies = map ((flip $ set pos) defaultEnemy) enemyPositions
  let player = set pos playerPosition defaultPlayer
  return GameState {_player = player, _enemies = enemies, _walls = walls}

app :: App GameState appEvent ()
app = App
  { appDraw = draw
  , appChooseCursor = showFirstCursor
  , appStartEvent = return
  , appAttrMap = const $ attrMap
    (white `on` black) -- default
    [("player", blue `on` black), ("enemy", red `on` black)]
  , appHandleEvent = eventHandler
  }

main :: IO ()
main = void $ makeState initialEnemyCount >>= defaultMain app
