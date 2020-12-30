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

aaa = verboseCheck (aTest :: [Int] -> Bool)
bTest xs = head (qsort xs) == minimum xs
bbb = quickCheck (bTest :: [Int] -> Bool)
-- fails
-- 0** Exception: Prelude.head: empty list
cTest xs = not (null xs) ==> head (qsort xs) == minimum xs
ccc = quickCheck (cTest :: [Int] -> Property)
cccV = verboseCheck (cTest :: [Int] -> Property)

-- https://www.futurelearn.com/info/courses/functional-programming-haskell/0/steps/27229

-- oh these are creative uses of quickCheck
ddd = quickCheck ((\n->(abs(n) == n) || (0-abs(n) ==n))::Int->Bool)
eee = quickCheck ((\l->(if l==[] then True else (minimum l) == (sort l)!!0))::[Int]->Bool)

