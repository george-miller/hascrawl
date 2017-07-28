-- Taken from here https://github.com/sulami/spielwiese/blob/master/astar/lib/Astar.hs and here https://github.com/sulami/spielwiese/blob/master/astar/src/Main.hs

module Astar (
  Grid, Coord, Path,
  PossibleWaysFun, CostFun,
  flood,
  main
  ) where

import Control.Parallel.Strategies (parMap, rpar)

type Grid = [String]
type Coord = (Int, Int)
type Path = [Coord]

type PossibleWaysFun = Grid -> Coord -> Path
type CostFun = Coord -> Path -> Int

flood :: Grid -> Coord -> Coord -> PossibleWaysFun -> CostFun -> Path
flood grid fin pos pwf cf = head $ fl grid fin pwf cf [[pos]]
  where
    fl :: Grid -> Coord -> PossibleWaysFun -> CostFun -> [Path] -> [Path]
    fl grid fin pwf cf paths
      | any (\p -> last p == fin) paths = filter (\p -> last p == fin) paths
      | otherwise = let best = snd $ minimum
                               $ zip (parMap rpar (cf fin) paths) paths
                        pb = addRoutes grid paths best pwf
                    in fl grid fin pwf cf $ filter (/= best) paths ++ pb

    addRoutes :: Grid -> [Path] -> Path -> PossibleWaysFun -> [Path]
    addRoutes grid ps path pwf = let cps = concat ps in
      [ path ++ [p] | p <- filter (`notElem` cps) $ pwf grid $ last path ]

possibleWays :: PossibleWaysFun
possibleWays g (x,y) = [ (x1,y1) | y1 <- [(y-1)..(y+1)],
                                   y1 >= 0,
                                   y1 < length g,
                                   x1 <- [(x-1)..(x+1)],
                                   x1 >= 0,
                                   x1 < length (g !! y1),
                                   x-x1 == 0 || y-y1 == 0,
                                   g !! y1 !! x1 /= 'X' ]

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

drawPath :: Grid -> Path -> Grid
drawPath = foldr (\(x,y) r -> replace r x y '*')

replace :: [[a]] -> Int -> Int -> a -> [[a]]
replace o x y c = let (rpre,rpost) = splitAt y o
                      row = head rpost
                      (cpre,cpost) = splitAt x row
                  in rpre ++ [cpre ++ [c] ++ tail cpost] ++ tail rpost

main = do grid <- fmap lines getContents
          let start = find grid 'S' 0
          let fin = find grid 'F' 0
          let path = flood grid fin start possibleWays cost
          mapM_ putStrLn $ drawPath grid path
