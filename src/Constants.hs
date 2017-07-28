{-#  LANGUAGE ScopedTypeVariables #-}
module Constants where
import Types

boardHeight :: Int = 40
boardWidth :: Int = 40
startingRooms :: Int = 6

playerCharacter :: String = "P"
wallCharacter :: String = "X"
enemyCharacter :: String = "E"

defaultPlayer :: MovingUnit = MovingUnit {
  pos = Point (-1) (-1),
  attack = 5,
  health = 10
                                         }
