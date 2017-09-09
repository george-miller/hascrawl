{-# LANGUAGE TemplateHaskell #-}
module Types where
import           Control.Lens

class Positionable a where
  position :: a -> Point

data Point = Point {x :: Int, y :: Int} deriving (Eq, Show)
instance Positionable Point where
  position = id

data MovingUnit = MovingUnit {_pos :: Point, _health :: Int, _attack :: Int} deriving (Eq, Show)
instance Positionable MovingUnit where
  position = _pos


data GameState = GameState {
  _player  :: MovingUnit,
  _enemies :: [MovingUnit],
  _walls   :: [Point]
                           } deriving (Eq, Show)

makeLenses ''MovingUnit
makeLenses ''GameState
