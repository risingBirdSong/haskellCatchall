import System.IO
import Data.List

matchHouses :: (Num p, Ord p) => p -> p
matchHouses x 
  | x == 1 = 6
  | x > 1 = 6 + ((x-1) * 5)


  -- Add up the Numbers from a Single Number

addUp x = sum [1..x]