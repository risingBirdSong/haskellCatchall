import System.IO
import Data.List

matchHouses x 
  | x == 1 = 6
  | x > 1 = 6 + ((x-1) * 5)
