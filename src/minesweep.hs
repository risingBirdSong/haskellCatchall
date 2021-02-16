module Minesweeper where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import System.Random

data Flag = Flag | Close | Open

type Number = Int

mine :: Number
mine = 9
empty :: Number
empty = 0

type Config = [[Bool]]
type Over = [[Flag]]
type Under = [[Number]]
type Coord = (Int, Int)
data Minesweeper = Minsweeper { boardWidth :: Int, boardHeight :: Int,
                                boardTotalMines :: Int, boardFlags :: Over,
                                boardNumbers :: Under }

member :: (Eq a) => a -> [a] -> Bool
member x [] = False
member x (y:ys) | x == y = True
                | otherwise = member x ys

-- 2D Functions

width :: [[a]] -> Int
width = length

height :: [[a]] -> Int
height = length . (!! 0)

inZone :: [[a]] -> Int -> Int -> Bool
inZone xss x y = (x `member` [0..(width xss - 1)]) && (y `member` [0..(height xss - 1)])

arounds :: [[a]] -> Int -> Int -> [Coord]
arounds xss x y = filter (uncurry $ inZone xss) [ (x + x', y + y') | x' <- [-1..1], y' <- [-1..1], x' /= 0 || x' - y' /= 0 ]

box :: [[a]] -> Int -> Int -> [Coord]
box xss x y = (x, y):(arounds xss x y)

new2D :: (Int -> Int -> a) -> Int -> Int -> [[a]] 
new2D f x y = [ [ f x' y' | y' <- [0..y - 1] ] | x' <- [0..x - 1] ]

init2D :: Int -> Int -> a -> [[a]]
init2D w h a = new2D (const . const a) w h

mapi2D :: (Int -> Int -> a -> b) -> [[a]] -> [[b]]
mapi2D f xss = new2D (\x y -> f x y $ at xss x y) (width xss) (height xss)

at :: [[a]] -> Int -> Int -> a
at xss x y = xss !! x !! y

-- Board Functions

randomCoord :: Int -> Int -> StdGen -> (Coord, StdGen)
randomCoord w h g =
  let (r1, g') = randomR (0, w - 1) g
      (r2, g'') = randomR (0, h - 1) g'
  in ((r1, r2), g'')

randomCoordExcept :: [Coord] -> Int -> Int -> StdGen -> (Coord, StdGen)
randomCoordExcept ts w h g =
  let (t, g') = randomCoord w h g
  in
    if t `member` ts
    then randomCoordExcept ts w h g'
    else (t, g')

randomCoords :: [Coord] -> Int -> Int -> Int -> StdGen -> ([Coord], StdGen)
randomCoords ts w h 0 g = ([], g)
randomCoords es w h m g =
  if w * h <= m then
    error ("randomCoords: Domain too small. ("
           ++ (show w) ++ " * " ++ (show h) ++ " <= " ++ (show m) ++ ")")
  else
    let (t, g') = randomCoordExcept es w h g
        (ts, g'') = randomCoords (t:es) w h (m - 1) g'
    in (t:ts, g'')

makeBoard :: [Coord] -> Int -> Int -> Int -> StdGen -> ([[Bool]], StdGen)
makeBoard ts w h m gen =
  let (coords, gen') = randomCoords ts w h m gen
  in (new2D (curry (`member` coords)) w h, gen')

--labelBoard :: [[Coord]] -> [[Number]]
labelBoard b =
  mapi2D (\x y i ->
    if at b x y
    then 9
    else
      let as = arounds b x y
      in (length . filter id . (map $ uncurry $ at b)) as
  ) b

basicmap = init2D 4 4 True