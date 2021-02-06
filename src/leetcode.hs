import Debug.Trace
import qualified Data.Set as S
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Char
import Control.Monad
import qualified Data.Map as M
import Data.List.Split
import Data.Ratio
import  System.Random
import Test.QuickCheck
import Data.Function
import Data.Bool
import Data.List.Split
import Data.Universe.Helpers
-- import Numeric.Probability.Percentage

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

-- insrtRT (Just v) =    

testRose = RT 1 [RT 2 [RT 3 [RT 5 []]], RT 4 [] ]

-- fromSortedList :: (Eq a) => [[a]] -> RT a

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

-- https://leetcode.com/problems/maximum-product-of-two-elements-in-an-array/
-- 1464. Maximum Product of Two Elements in an Array

mxpr = product . map (subtract 1) . take 2 . reverse . sort 


-- https://leetcode.com/problems/n-repeated-element-in-size-2n-array/
-- 961. N-Repeated Element in Size 2N Array

therepeat xs = head . last . sortBy (comparing length) . group . sort $ xs


-- data RT a = No | Rose a [RT a]

-- showTreeLR RT 

-- https://leetcode.com/problems/array-partition-i/
-- 561. Array Partition I

arrPart xs = sum $ go s
      where s =  sort xs
            go [] = []
            go (x:y:xs) = min x y : go xs  

arrPart' = sum . mapPair min . sort

-- mapPair -- process a list two at a time with a function, do not place the y value back into the list for the next recursive call
-- (a pattern i do regularly when I want the y, of my x y data needs to be the first arg in the next recursion) also must be even length to work
mapPair _ [] = [] 
mapPair _ [] = [] 
mapPair f (x:y:ls) = f x y : mapPair f ls 

mapTw f [] = []
mapTw f xs = f (take 2 xs) : mapTw f (drop 2 xs)   

-- https://leetcode.com/problems/peak-index-in-a-mountain-array/
pk xs = go xs (head xs) where
  go [] _ = 0 
  go (x:xs) hi 
    | x > hi = 1 +  go xs x
    | otherwise  =  go xs hi

pk' xs = snd . maximum . zip xs $ [0..]


-- https://leetcode.com/problems/unique-number-of-occurrences/
-- length seti == length grouped
unq xs = length seti == length grouped
    where seti = S.fromList xs
          grouped = S.fromList $ map length $ group $ sort xs

-- https://leetcode.com/problems/minimum-cost-to-move-chips-to-the-same-position/
cost trg n 
  | abs (trg - n) == 1= 1
  | otherwise = 0 

chips xs = sum $ map (cost trg) $ toMove 
    where trg = head $ maximumBy (compare `on` length) $ group xs
          toMove = filter (/= trg) xs


-- chipanswer :: [Integer]
chipanswer = chips [2,2,2,3,3]
-- chipanswer = chips [[2,2,2],[3,3]]

-- [3,5,1]

arithT = [3,5,1]
arithF = [1,2,4]

second (x:y:_) = y

-- https://leetcode.com/problems/can-make-arithmetic-progression-from-sequence/
arth xs = compy == sort xs
  where srtd = sort $ xs
        strt = head srtd 
        growth = head $ drop 1 $ srtd
        compy = [strt , growth .. last srtd]

play =  [4,3,10,9,8]

minSub xs = taking [] $ desc  
    where desc = reverse $ sort xs 
          sm = sum $ xs

-- https://leetcode.com/problems/minimum-subsequence-in-non-increasing-order/
taking xs (r:rest) 
  | sum (xs) <= sum (r:rest) = taking (r:xs) rest
  | otherwise  = (xs,r:rest)


minSub' xs = go [] $ sort xs
  where go tk (d:dp) 
          | sum (d:tk) < sum dp = go (d:tk) dp 
          | otherwise = d:dp



minSub''' = fmap snd . (find <$> (. fst) . (<) . (`div` 2) . sum <*> (zip <$> scanl (+) 0 <*> inits)) . reverse . sort

zippingSumInits = zip <$> scanl (+) 0 <*> inits $ [1,2,3,4]

-- https://leetcode.com/problems/maximum-units-on-a-truck/

toTuple [a,b] = (a,b)

truck xs n = sum $ go clean n
  where clean = sortBy ( flip  compare `on` last) xs
        go [] n = []
        go ([b,u]:bxs) n 
          | n <= 0 = []
          | otherwise  = u * min b n : go bxs (n-b)

truck' xs n = sum 
             . take n 
             . concatMap (uncurry replicate) 
             . sortOn (Down . snd)
             . map toTuple $ xs  

inp = [[5,10],[2,5],[4,7],[3,9]]

-- https://leetcode.com/problems/shortest-distance-to-a-character/

shrts xs c = 
    let zipd =  zip [0..] xs
        targets = filter (\(idx,ltr) -> ltr == c) zipd
          -- cands == candidates ... cur == current ... dst == distance
        minDist cands cur = minimum $ map (\(dst, lt) -> abs (dst - cur)) cands
    in map (minDist targets) $ map fst zipd  

-- morrows cool answer
shrts' xs c = zipWith min (tail $ go (scanl' . flip)) (init $ go scanr)
  where
    go dir = dir (bool (+1) (const 0) . (==c)) (maxBound - length xs) xs


minDistLtr cands cur = minimum $ map (\(dst, lt) -> abs (dst - cur)) cands

ees = [(3,'e'),(5,'e'),(6,'e'),(11,'e')]


ff (b, u) (n, _) = let s = max 1 (min n b) in (n - s, u * s)
trucking n = sum . map snd . takeWhile ((>= 0) . fst) . scanl ff (n, 0) . sortBy (comparing $ Down . snd)

truckinp = [(1,3),(2,2),(3,1)]
truckingA cargo limit =  scanl ff (limit, 0) . sortBy (comparing $ Down . snd) $ map toTuple cargo


palitest = "hellosannasxyz"
-- lngstPaliSuba :: (Ord a, Ord b, Num b, Enum b) => [a] -> [[(a, b)]]
lngstPaliSuba xs = sortOn snd $ concat . takeWhile ((>=2).length) . sortOn (Down . length) . groupBy ((==) `on` fst) . sort $ zip xs [0..]
lngstPaliSubb xs = sortBy (comparing length) . groupBy ((==) `on` fst) . sort $ zip xs [0..]
-- lngstPaliSuba xs = groupBy (\(v,i) -> (==(v))  ) . sort $ zip xs [0..]

-- consecutives a b 
--   | (a+1) == b = True 
--   | otherwise = False  

groupNum ns = groupBy (\a -> \b -> (a + 1) == b ) ns 

groupedNumsInput = [1,2,3,5,6,7,9,10,11]
groupedNumsOutput = [[1,2,3],[5,6,7],[9,10,11]]

fff=maximum.map length.group.zipWith(-)[1..]

-- https://gitlab.haskell.org/ghc/ghc/-/issues/1408

groupConsecutive :: (Ord a, Num a) => [a] -> [[a]]
groupConsecutive = foldr group []
    where 
		group x [] = [[x]]
		group x acc@((h:t):rest)
			| h - x <= 1 = (x:h:t):rest
			| otherwise = [x]:acc

main :: IO()
main = print $ groupConsecutive [1, 2, 4, 5, 6, 9 :: Int]


trgtDistances xs trg = map fst . filter (\(idx, str) -> str == trg) $ zip [0..] xs
shortDHelper xs trg = minimum $ map (\x -> abs (trg - x)) xs

shortD xs trg = map (shortDHelper (trgtDistances xs trg)) $ zipWith const [0..] xs


-- *Main> shrtD "loveleetcode" 'e'
-- [(3,'e'),(5,'e'),(6,'e'),(11,'e')]

-- 509. Fibonacci Number

-- naive implementation
fby 1 = 1 
fby 0 = 1
fby n = fby (n-1) + fby (n-2)

fibsn = 0 : 1 : zipWith (+) fibsn (tail fibsn)


-- 1200. Minimum Absolute Difference

mad xs = minimum $ go srtd
  where go [x] = []
        go (x:y:rest)= (y-x) : go (y:rest) 
        srtd = sort xs
         

madSolver xs minn = go $ sort xs
  where go [x] = []
        go (x:y:ls) 
          | (y-x) == minn = [x,y] : go (y:ls) 
          | otherwise  = go (y:ls)
        
madSolv xs = madSolver xs (mad xs)


-- https://leetcode.com/problems/single-number/
-- 136. Single Number
sngleN xs = safeHead . concat . filter ((==1).length) $ group $ sort xs

safeHead (x:_) = Just x 
safeHead  [] = Nothing 

-- 766. Toeplitz Matrix
mymatrix = [[1,2,3,4],[5,1,2,3],[9,5,1,2]]
toepM xxs = allEqual $ map allEqual $ diagonals $ reverse xxs

allEqual [] = True 
allEqual (x:xs) = all (==x) xs

-- sampersand made a nice implementation for the island perimeter problem in Ruby 
-- require 'set'
-- def doit(grid)
--     land = Set.new

--     grid.each_with_index do |eles, row|
--         eles.each_with_index do |ele, col|
--             land.add(row + 1i*col) unless ele.zero?
--         end
--     end

--     land
--         .map { |z| 4 - [z+1, z-1, z+1i, z-1i].count { |x| land.include? x } }
--         .sum
-- end


-- puts doit [[0,1,0,0],[1,1,1,0],[0,1,0,0],[1,1,0,0]]


stayPositiveTest = [-3,2,-3,4,2]

-- 1413. Minimum Value to Get Positive Step by Step Sum
-- https://leetcode.com/problems/minimum-value-to-get-positive-step-by-step-sum/

stayPos xs 
  | minnie > 0 = 1
  | otherwise = 1 + abs minnie 
  where minnie =  minimum $ scanl (+) 0 xs

-- 806. Number of Lines To Write String
nlws xs 
  | length xs `rem` 10 == 0 = dvmd
  | otherwise = go dvmd
  where dvmd = length xs `divMod` 10
        go (y,x) = (y+1,x)


toBin 0 = [0]
toBin n = reverse (helper n)
helper 0 = []
helper n = let (q,r) = n `divMod` 2 in r : helper q


-- 476. Number Complement
numComp n =  map binFlip binn 
  where binn = toBin n 

binFlip n 
  | n == 1 = 0
  | n == 0 = 1
  | otherwise = error "only works with binary nums"


-- 1619. Mean of Array After Removing Some Elements
maarse xs = (thesum, thelength, thesum / thelength)
    where fve = floor $ realToFrac (length (xs)) * 0.05
          rmvd = drop fve $ reverse $ drop fve $ sort xs
          thesum = realToFrac $ sum rmvd 
          thelength = realToFrac $ length rmvd


-- 349. Intersection of Two Arrays
-- solve just by using Set data structure
ita as bs = S.intersection (S.fromList as) (S.fromList bs)

-- 349. Intersection of Two Arrays
-- try again without using interesection

-- ita' xs ys = 
--   where go _ [] = []
--         go Nothing 

includes ys x = find ((==)x) ys 

ita' xs ys = filter keepJusts $ map (includes (nub ys)) (nub xs)

-- keepJusts = []
keepJusts Nothing = False  
keepJusts (Just x) = True 


-- 1455. Check If a Word Occurs As a Prefix of Any Word in a Sentence

cwop ws srch 
  | null candids = -1
  | otherwise  = fst $ head candids
     where candids = filter (\(i,w) -> prefix srch w) $ zip [1..] $ words ws

prefix qry wrd = qry == take (length qry) wrd

-- 1598. Crawler Log Folder
crawler ins = max 0 $ sum $ map logic ins
    where logic "../" = (-1)
          logic "./" = (0)  
          logic _ = 1

-- 884. Uncommon Words from Two Sentences
uncommons as bs = concat 
                . filter ((==1) . length) 
                . group 
                . sort 
                $ words (as ++ " " ++ bs)

--  Sort Array by Increasing Frequency
saif xs = concat $ sortOn length . sortOn Down . group $ sort xs

-- 383. Ransom Note

rnsnt source candidate = all (`elem` s_cand) s_src
    where s_src = group $ sort source 
          s_cand = group $ sort candidate 


-- 1005. Maximize Sum Of Array After K Negations
msaakn xs c = sum $ go xs c 
  where go [] _ = [] 
        go xs 0 = xs 
        go xs n = go (ngtSmallest $ sort xs) (n-1)

ngtSmallest (x:xs) = (negate x) : xs  

-- 1089. Duplicate Zeros

ddppzzss = [1,0,2,3,0,4,5,0]

dpzs xs = take (length xs) $ go xs 
  where go [] = []
        go (0:xs) = 0 : 0 : go xs
        go (x:xs) = x : go xs


-- 485. Max Consecutive Ones

mco ns = sum . maximum . sort $ group ns

hola ns = sort $ group ns

-- *Main> mco [1,1,1,1,0,1,1,1,0,1,1,1,1,1,1]
-- [[0],[0],[1,1,1],[1,1,1,1],[1,1,1,1,1,1]]

-- 409. Longest Palindrome

ploss = "abccccddeee"
mkLnPl str = group $ sort str
preppped :: [[Char]]
preppped = mkLnPl ploss

aCenter srtd = fromMaybe "" $ find ((==1).length) srtd
therest srtd = dropWhile ((==1).length) srtd 

evnOddHndler xs 
  | even $ length xs = xs
  | otherwise = drop 1 xs 

paliSolver xs = (stitch rest) ++ first ++ (reverse $ stitch rest)
    where first = aCenter prepped
          rest = therest preppped 
          prepped = group $ sort xs   
          stitch xss = concat $ map (\x -> take (length x `div` 2) x) xss


-- Pascal's Triangle
ptA = [0,1,0]
ptB = [0,1,1,0]
ptC = [0,1,2,1,0]
ptD = [0,1,3,3,1,0]
ptE = [0,1,4,6,4,1,0]

pascalsTriangle = solver [0,1,0]
 where go (x:y:xs) = x + y : go (y:xs)
       go [x] = []
       single xs = 0 : go xs ++ [0]
       solver xs = xs : solver (single xs) 

