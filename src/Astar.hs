-- Taken from here https://github.com/sulami/spielwiese/blob/master/astar/lib/Astar.hs and here https://github.com/sulami/spielwiese/blob/master/astar/src/Main.hs

module Astar (
  Grid, Coord, Path,
  PossibleWaysFun, CostFun,
  nextPlace
  ) where

import           Constants
import Control.Lens
import           Control.Parallel.Strategies (parMap, rpar)
import Types

type Grid = [String]
type Coord = (Int, Int)
type Path = [Coord]

type PossibleWaysFun = Coord -> Path
type CostFun = Coord -> Path -> Int

getUnavailiblePoints :: GameState -> [Point]
getUnavailiblePoints state =
  let movables = _player state : _enemies state
  in _walls state ++ map (view pos) movables

convertPointToCoord :: Point -> Coord
convertPointToCoord (Point x y) = (x,y)

nextPlace :: GameState -> Point -> Point
nextPlace state start =
  let
    waysFun = Astar.possibleWays $ map convertPointToCoord $ getUnavailiblePoints state
    (x, y) = head $ Astar.flood (convertPointToCoord $ view (player . pos) state) (convertPointToCoord start) waysFun Astar.cost
  in Point x y

flood :: Coord -> Coord -> PossibleWaysFun -> CostFun -> Path
flood fin pos pwf cf = head $ fl fin pwf cf [[pos]]
  where
    fl :: Coord -> PossibleWaysFun -> CostFun -> [Path] -> [Path]
    fl fin pwf cf paths
      | any (\p -> last p == fin) paths = filter (\p -> last p == fin) paths
      | otherwise = let best = snd $ minimum
                               $ zip (parMap rpar (cf fin) paths) paths
                        pb = addRoutes paths best pwf
                    in fl fin pwf cf $ filter (/= best) paths ++ pb

    addRoutes :: [Path] -> Path -> PossibleWaysFun -> [Path]
    addRoutes ps path pwf = let cps = concat ps in
      [ path ++ [p] | p <- filter (`notElem` cps) $ pwf $ last path ]

possibleWays :: [Coord] -> PossibleWaysFun
possibleWays unavailablePoints (x,y) =
  [ (x1,y1)
    | y1 <- [(y-1)..(y+1)],
       x1 <- [(x-1)..(x+1)],
       y1 >= 0,
       y1 < boardHeight,
       x1 >= 0,
       x1 < boardWidth,
       x-x1 == 0 || y-y1 == 0,
       (x1, y1) `notElem` unavailablePoints]

cost :: CostFun
cost fin path = let l = last path in (length path - 1) + (dist l fin)
  where
    dist :: Coord -> Coord -> Int
    dist (x0,y0) (x1,y1) = abs (x0-x1) + abs (y0-y1)

find :: Grid -> Char -> Int -> Coord
find (x:xs) c n | c `elem` x = (f' x c 0, n)
                | otherwise  = find xs c $ n+1
  where
    f' :: String -> Char -> Int -> Int
    f' (x:xs) c n | c == x    = n
                  | otherwise = f' xs c $ n+1
