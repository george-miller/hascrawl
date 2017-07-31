{-#  LANGUAGE ScopedTypeVariables #-}
module Constants where
import Types

boardHeight :: Int = 30
boardWidth :: Int = 30
startingRooms :: Int = 7
initialEnemyCount :: Int = 4

playerCharacter :: String = "P"
wallCharacter :: String = "X"
enemyCharacter :: String = "E"

defaultPlayer :: MovingUnit = MovingUnit {
  _pos = Point 0 0,
  _attack = 5,
  _health = 10
                                         }

defaultEnemy :: MovingUnit = MovingUnit {
  _pos = Point 0 0,
  _attack = 2,
  _health = 5
                                         }
