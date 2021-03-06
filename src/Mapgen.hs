module Mapgen where
-- Taken from here https://gist.github.com/splinterofchaos/2874811
import System.Random

import System.Console.GetOpt
import System.Environment(getArgs, getProgName)
import Constants
import Types

type Coord = (Int,Int)
type Range = (Int,Int)
type Area  = (Coord,Coord) -- Upper-left and lower-right bounds.

data Tile = TFloor | TWall
instance Show Tile where
  show TFloor = "."
  show TWall  = "#"

type MRow = [Tile]
type RMap = [MRow]

showMap :: RMap -> String
showMap = unlines . map (>>= show)

makeMap :: Coord -> RMap
makeMap (x,y) = replicate y (replicate x TWall)

splitGap :: Int -> Int -> [a] -> ([a],[a],[a])
splitGap start size lst = (before, middle, after)
  where
    (before,rest) = splitAt start lst
    (middle,after) = splitAt (abs size) rest

digRow :: Range -> MRow -> MRow
digRow (start,end) row =
  before ++ replicate size TFloor ++ after
  where
    size = end - start + 1
    (before,_,after) = splitGap start size row

digRoom :: RMap -> Area -> RMap
digRoom rmap ((x,y),(u,v)) =
  ybefore ++ map (digRow (x,u)) rows ++ yend
  where
    (ybefore,rows,yend) = splitGap y (v-y+1) rmap

randomRoom :: (RandomGen r) => r -> Coord -> Area
randomRoom gen (w,h) =
  ((x',y'),(u',v')) -- Note the reordering of xuyv to xyuv.
  where
    -- Here, x = n, so start with a random n.
    [x,y,u,v] = take 4 . map fst $ iterate (next.snd) (n,g)
    (n,g) = next gen

    (x',u') = to_range x u w
    (y',v') = to_range y v h
    to_range a b max = (a',b')
      where
        minlen = 3
        a' = a `mod` (max-minlen-1) + 1
        brange = max - a' - minlen
        b' = (if brange > 0 then b `mod` brange else 0)
             + a' + minlen - 1

randomPoint :: RandomGen r => Area -> r -> Coord
randomPoint ((x,y),(u,v)) gen =
  (x' `mod` (u-x+1) + x, y' `mod` (v-y+1) + y)
  where (x',g) = next gen
        (y',_) = next g

randomRooms :: RandomGen r => r -> Coord -> [Area]
randomRooms gen dims = randomRoom g1 dims : randomRooms g2 dims
  where (g1,g2) = split gen

digHallway :: RMap -> Area -> RMap
digHallway m ((x,y),(u,v)) = foldl digRoom m
  -- Dig from (x,y) to (u,y) to (u,v).
  [((u,min y v),(u,max y v)),((min x u,y),(max x u,y))]

digRandomHallways :: RandomGen r =>
                     RMap -> r -> [Area] -> RMap
digRandomHallways m gen rooms
  | length rooms < 2 = m
  | otherwise =
    digRandomHallways m' g4  (tail rooms)
  where
    (g1, gx) = split gen
    (g2, g3) = split gx
    ends = (randomPoint (rooms!!0) g1, randomPoint (tail rooms!!n) g2)
    m' = digHallway m ends
    (n',g4) = next g3
    n = n' `mod` (length $ tail rooms)

splatter :: RandomGen r => Int -> r -> RMap -> RMap
-- Splatter n random rooms onto m.
splatter n gen m =
  digRandomHallways (foldl digRoom m rooms) g2 rooms
  where
    (g1,g2) = split gen
    rooms = take n $ randomRooms g1 (length (m!!0),length m)
    center ((x,y),(u,v)) = ((x+u) `quot` 2, (y+v) `quot` 2)

convertRMap :: RMap -> [Point]
convertRMap rmap = let
    doCol [] _ _ = []
    doCol (item:col) rowIndex colIndex = case item of
      TWall -> Point colIndex rowIndex : doCol col rowIndex (colIndex+1)
      _ -> doCol col rowIndex (colIndex+1)
    doRow [] _ = []
    doRow (row:world) rowIndex = doCol row rowIndex 0 : doRow world (rowIndex+1)
  in concat $ doRow rmap 0

genWalls :: IO [Point]
genWalls = do
  gen <- newStdGen
  return $ convertRMap $ splatter startingRooms gen $ makeMap (boardWidth, boardHeight)

m :: IO ()
m = do
  gen <- newStdGen
  putStrLn $ showMap $ splatter startingRooms gen $ makeMap (boardWidth, boardHeight)
