module Types where

class Positionable a where
  position :: a -> Point

data Point = Point {x :: Int, y :: Int}
instance Positionable Point where
  position = id

data MovingUnit = MovingUnit {pos :: Point, health :: Int, attack :: Int}
instance Positionable MovingUnit where
  position = pos

data GameState = GameState {
  player :: MovingUnit,
  enemies :: [MovingUnit],
  walls :: [Point]
                           }
