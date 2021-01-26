import Debug.Trace
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Char
import Control.Monad
import qualified Data.Map as M
import Data.List.Split
import  System.Random
import Test.QuickCheck

import Data.List.Split


import Data.Ord

a = [1,1]
b = [2,2]
c = [1,2] 
d = [2,3]

e = [2]
f = [1,3]

g = [1,2,5]
h = [2,4]

fairCandy as bs = go
  where avg = (sum as + sum bs) `div` 2
        go = [(a,b) | a <- as, b <- bs, ((sum as) - a + b) == avg]
             


smaller as bs = head $ filter (\x -> (sum x) == (min (sum as) (sum bs))) [as,bs]

smaller_ as bs 
  | sum as < sum bs = as
  | otherwise = bs 

theOtherList theOne choice@[as,bs] = head $ filter ((/=) theOne ) choice 


-- Reverse Only Letters
rol input = go input onlylttrs 
    where onlylttrs = reverse $ filter (isAlpha) input
          go [] _ = []
          go ori [] = [] ++ ori
          go (a:all) (l:ltrs) 
              | (not . isAlpha) a = a : go all (l:ltrs)
              | otherwise = l : go all ltrs 

onlyLettersRev input = reverse $ filter (isAlpha) input
  
lettertest = "a-bC-dEf-ghIj"

lttrHandler [] _ = []
lttrHandler _ [] = []
lttrHandler (a:all) (l:ltrs) 
    | (not . isAlpha) a = a : lttrHandler all (l:ltrs)
    | otherwise = l : lttrHandler all ltrs 



-- rol ltrs = myDelim ltrs (notAlphas ltrs)
myDelim lst dlm = go lst dlm []
  where go [] dlm acc = [acc]
        go (a:ls) dlm acc
          | a `elem` dlm = acc : [a] : go ls dlm []
          | otherwise = go ls dlm (acc++[a]) 


-- Thousand Separator
thssep num = reverse . intercalate "." . chunksOf 3 . reverse $ show num


-- example of custom sort 
subsortGT a b
  | a <= b = GT
  | a > b = LT

sortGT (a1, b1) (a2, b2)
  | a1 + b1 < a2 + b2 = GT
  | a1 + b1 > a2 + b2 = LT
  | a1 + b1 ==  a2 + b2= subsortGT a1 a2

sortetest = sortBy sortGT [(0,1),(0,3),(1,1),(1,3),(2,0),(2,1),(2,3),(4,1),(4,2)]


mveZeroes a b 
  | a == 0 = GT 
  | otherwise = LT 

movedz ls = sortBy mveZeroes ls

-- Add Digits
addDgts dig 
        | (adder dig ) < 10 = adder dig 
        | otherwise = adder (adder dig)
  where adder val = sum $ map (digitToInt) $ show val


-- impEmply emp id = dropWhile (\x -> head x /= id ) emp
-- this will break because of the invalid type, and needs a different data type like
-- a tree... which i'll implement, but was wondering how to go about parsing something like 
-- this, unsafe, untyped data from something like an API. how to go about cleaning it
-- up to use with haskell?
-- empTest = [ [1, 5, (2, 3)], [2, 3, ], [3, 3, ]]
data Item a = One a | Many [Item a] deriving (Show, Eq, Ord)

empTest =  [ Many [One 1, One 5, Many [One 2,One 3]], Many [One 2, One 3, Many []], Many [One 3,One 3, Many []]]

manyAccess (Many x) = x
manyAccess _ = error "try different"
oneAcces (One x) = x


impEmply lst emp = dropWhile (\x ->  emp(/=)( empAccess x) ) lst

empAccess x = oneAcces $ head (manyAccess x) 

data RoseTree a = RoseTree a [RoseTree a] deriving (Show, Eq,Ord)
data RT a = RT a [RT a] deriving (Show, Eq, Ord)

stockinput = [7,1,5,3,6,4]
-- Output: 7

-- https://leetcode.com/problems/best-time-to-buy-and-sell-stock-ii/
stocking [] acc = acc 
stocking [x] acc = acc 
stocking (a:b:ls) acc 
  | b > a = stocking (b:ls) (acc +( b - a)) 
  | otherwise = stocking (b:ls) acc

stocksort tupa tupb 
    | ((snd tupa)-(fst tupa) > (snd tupb)-(fst tupb)) = GT 
    | ((snd tupa)-(fst tupa) < (snd tupb)-(fst tupb)) = LT  
    | otherwise  = EQ 

-- https://leetcode.com/problems/monotonic-array/
mntnc (a:b:lst) 
  | a < b = go (a:b:lst)
  | a > b = go (reverse (a:b:lst))
  where go [] = True 
        go [x] = True 
        go (a:b:ls) 
          | a <= b = go (b:ls)
          | otherwise = False 

rankT lst = map ((+1).(fromMaybe 0 .) (`S.lookupIndex` setsort)) lst
    where setsort = S.fromList lst


-- . Merge Two Sorted Lists
l1 = [1,2,4]
l2 = [1,3,4,5]
mrgtwosrt as bs =concatMap (\(x,y) -> [x,y]) $ zip as bs 

mrgtwosrtA as bs = go as bs
    where go [] bs = bs 
          go as []  = as 
          go (a:as) (b:bs)  = a : b : go as bs

-- Count Odd Numbers in an Interval Range
cntOddRange l h =  length $ filter  odd $ [l..h]

--  Find All Numbers Disappeared in an Array
ddd = [4,3,2,7,8,2,3,1]
eee = [4,6,8,9,11,14,20]
-- [2,3] [5] [7] [10] [12,13] [15,16,17,18,19]
dspr lst = go (minimum lst) cleaned 
    where cleaned = (nub $ sort lst)
          go cnt [] = []
          go cnt (a:as) 
              | cnt == a = go (succ cnt) (as)
              | cnt < a = [cnt..a-1] : go (a) (a:as) 

dspra lst = go (nub . sort $ lst)
    where go [] = []
          go [x] = []
          go (a:b:ls) 
            | a == (b-1) = go (b:ls)
            | otherwise = [a+1..b-1] : go (b:ls)  


richestCust xs = sum $ maximum xs 

sortedRich xs = sum $ last $ sortOn sum  xs

-- richCompare :: (NonEmptyList [[Integer ]]) -> Bool
richCompare (NonEmpty xs) = (sortedRich xs ) == (richestCust xs)

-- richTest = quickChec (richCompare )

-- 1021. Remove Outermost Parentheses

l x = x == '('
r x = x == ')'

inner x y = (l x) && (r y)    
-- pf inner = (. r) . (&&) . l

-- rmvOut ps = nubBy (\x y -> if inner x y == True then [x,y] else []) ps

iffy = (\x y -> if inner x y == True then [x,y] else [])

filterTwo f [] = [] 
filterTwo f [x] = [] 
filterTwo f (x:y:ls)
  | (f x y) = x : y : filterTwo f (y:ls)
  | otherwise = filterTwo f (y:ls)


minTimeVisitAll xs = sum . dif . map (maximum) $ xs

dif (x : []) = []
dif (x : y : xs) = (abs (x - y)) : dif (y:xs)


-- maxDistance (frst : scnd : []) = [] 
-- maxDistance (frst : scnd : rest ) = (max ((head frst - head scnd)) (abs (last scnd) - last scnd) ) : maxDistance (scnd : rest)


-- example1 =  "( )())()"
-- output1 =  "() : recurse ()()"


innerMostWork [] = []
innerMostWork ([x]) = []
innerMostWork (x : y : rest) 
  | x == '(' && y == ')' = "()" : innerMostWork rest  
  | otherwise = innerMostWork (y:rest) 

innerMost xs = concat $ innerMostWork xs

minDist xs = sum $ go xs where
  go [x] = []
  go ([x,y]:[x1,y1]:rest) =
    max (abs (x - x1)) (abs (y - y1)) : go ([x1,y1]:rest) 


-- pattern match test
tester (x:y:rest) = ([x,y], rest)


randos = do
  g <- newStdGen
  return $ map (`div` 100000000000000000) $ take 2 (randoms g :: [Int])
  
gnerater = do replicateM 5 randos

solver = do
  gened <- gnerater
  print gened
  return $ minDist gened
  

-- 1588. Sum of All Odd Length Subarrays

subarrs n [] = [] 
subarrs n xs 
  | (length $ take n xs) /= n = []
  | otherwise = (take n xs) : subarrs n (drop 1 xs) 

subarrsolver xs = sum $ map sum $ concatMap (`subarrs` xs) [1,3,5]

-- print $ splitEvery 3 [1..5]

splitEvery' :: Int -> [a] -> [[a]]
splitEvery' n = takeWhile (not.null) . map (take n) . iterate (drop n)

discount [] = []
discount (x:xs) = giveDiscount x (find (<=x) xs) : discount xs 

giveDiscount x Nothing = x
giveDiscount x (Just y) = x - y 

asfda = fmap (subtract 2 ) (Just 4)

-- https://leetcode.com/problems/find-n-unique-integers-sum-up-to-zero/
sumToZero n = [(-n)..n]