import Test.QuickCheck
import Data.List

-- http://book.realworldhaskell.org/read/testing-and-quality-assurance.html

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

aTest xs = qsort (qsort xs) == qsort xs
-- *Main> verboseCheck aTest
-- Passed:
-- []
-- Passed:
-- []
-- Passed:
-- [()]
-- Passed:
-- []
-- Passed:
-- [(),()]
-- Passed:
-- [()]
-- Passed:
-- [()]
-- Passed:
-- [(),(),(),(),()]