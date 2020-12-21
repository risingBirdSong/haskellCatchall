import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

diagonalDiffPrimary lst = go 0 []
  where go idx acc
         | idx == Data.List.length lst = acc
         | otherwise =  go (succ idx) (( (!!) ((!!) lst idx ) idx ) : acc)

diagonalDiffSecondary lst = go 0 [] 
  where go idx acc
         | idx == Data.List.length lst = acc
         | otherwise =  go (succ idx) (( (!!) ((!!) lst ((Data.List.length lst)-idx-1) ) idx ) : acc)

diagonalDifference lst = sum (diagonalDiffPrimary lst) + sum (diagonalDiffSecondary lst)