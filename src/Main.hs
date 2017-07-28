module Main where

import qualified Astar
import qualified Mapgen
import Brick

ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
main = simpleMain ui
-- main :: IO ()
-- main = Mapgen.main
