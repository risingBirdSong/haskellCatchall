import qualified Data.Matrix as M
import Data.List

import Data.Array 
image = [[1,1,1],[1,1,0],[1,0,1]]

type Point = (Int, Int)

lo, hi :: Array Point Int -> Point
lo a = fst $ bounds a
hi a = snd $ bounds a

outOfBounds, emptyPoint, takenPoint :: Array Point Int -> Point -> Bool
outOfBounds a (x, y)
  | x < 0 || x > fst (hi a) = True
  | y < 0 || y > snd (hi a) = True
  | otherwise = False
emptyPoint a p = a ! p < 0
takenPoint a p = a ! p > 0


floodFill :: Int -> Point -> Array Point Int -> Array Point Int
floodFill r i a
  | outOfBounds a i = a
  | emptyPoint a i  = a
  | takenPoint a i  = a
  | a ! i == 0 = floodFill r (x+1, y) .
                 floodFill r (x-1, y) .
                 floodFill r (x, y+1) $
                 floodFill r (x, y-1) ua
  where (x, y) = i
        ua = a // [(i, r)]

