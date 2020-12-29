import Test.QuickCheck
import Data.List
import Debug.Trace
prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

-- https://www.schoolofhaskell.com/user/pbv/an-introduction-to-quickcheck-testing
mainA = quickCheck prop_revapp

mySplit del [] = []
mySplit del list =  takeWhile (/=del) list : mySplit del (drop 1 (dropWhile (/=del) (list)) )

mySplit' d ls = tk : (if null dp then [] else  mySplit' d (tail dp) )
      where tk = takeWhile (/=d) ls
            dp = dropWhile (/=d) ls


split c [] = []
split c xs = xs' : if null xs'' then [] else split c (tail xs'')
    where xs' = takeWhile (/=c) xs
          xs''= dropWhile (/=c) xs

-- show
examples = [('@',"pbv@dcc.fc.up.pt"), ('/',"/usr/include")]

test (c,xs) = unwords ["split", show c, show xs, "=", show ys]
    where ys = split c xs

mainB = mapM_ (putStrLn.test) examples

unsplit :: Char -> [String] -> String
unsplit c = concat . intersperse [c]



prop_split_inv c xs = unsplit c (split c xs) == xs

maintest = quickCheck prop_split_inv

-- *Main> maintest
-- +++ OK, passed 100 tests.
-- *Main> maintest
-- +++ OK, passed 100 tests.
-- *Main> maintest
-- *** Failed! Falsified (after 90 tests and 6 shrinks):
-- '^'
-- "^"
-- *Main> maintest
-- *** Failed! Falsified (after 11 tests and 2 shrinks):
-- 'F'
-- "F"
-- *Main>





