{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Data.Char
import Data.Ord
import Data.List.Split
import qualified Data.List.GroupBy as Grp
import qualified Data.Map as M
import qualified Data.Vector as V
import Debug.Trace

--                                                    myLib
-- *****************************************************************************************************

count :: (Ord a, Integral b) => [a] -> M.Map a b
count =
  foldr updateMap M.empty
    where updateMap v counts
            | M.member v counts = M.adjust succ v counts
            | otherwise           = M.insert v 1 counts

-- *****************************************************************************************************

centuryFromYear y =  ((y-1) `div` 100) + 1

adjacentElementsProduct xs = maximum $ zipWith (*) xs (tail xs)

shapeArea n 
  | n == 1 = 1
  | otherwise = ((n-1) * 4) + shapeArea (n-1) 

makeArrayConsecutive2 n = length [(minimum n) .. (maximum n)] - length n


-- almostIncreasingSequence s = and $ ((<2) . length . filter (uncurry (>=)) . zip s) . tail <$> [s, tail s]


almostIncreasingSequence s = (filter (uncurry (>=)) . zip s) . tail <$> [s, tail s]

la = ["aba", 
 "aa", 
 "ad", 
 "vcd", 
 "aba"]

almstIncSeq xs = and $ ((<2) . length . filter (uncurry (>)) . zip xs) . tail <$> [xs, tail xs]

libgroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
libgroupBy _ [] = []
libgroupBy p' (x':xs') = (x' : ys') : zs'
  where
    (ys',zs') = go p' x' xs'
    go p z (x:xs)
      | p z x = (x : ys, zs)
      | otherwise = ([], (x : ys) : zs)
      where (ys,zs) = go p x xs
    go _ _ [] = ([], [])

libgroupByA _ [] = []
libgroupByA p' (x':xs') = (x' : ys') : zs'
  where 
    (ys',zs') = go p' x' xs' 
    go p z (x:xs)
       | p z x = (x : ys, zs)
       | otherwise = ([], (x:ys) : zs)
        where (ys,zs) = go p x xs 
    go _ _ [] = ([],[])

libgroupByB _ [] = []
libgroupByB p' (x':xs') = (x':ys') : zs'
  where (ys',zs') = go p' x' xs'
        go p z (x:xs)
           | p z x = (x:ys, zs)
           | otherwise = ([], (x:ys) : zs)
             where (ys, zs) = go p x xs
        go _ _ [] = ([],[])

libgroupByC _ [] = []
libgroupByC p' (a:as) = (a : xs') : zs' 
  where (xs', zs') = go p' a as 
        go p z (x:xs) 
          | p z x = (x:ys, zs)
          | otherwise = ([], (x:ys) : zs)
            where (ys, zs) = go p x xs 
        go _ _ [] = ([],[])

-- *Main> libgroupByC (<) [1,2,3,4,5,4,5,6,7,6,7,8,9]
-- [[1,2,3,4,5],[4,5,6,7],[6,7,8,9]]

-- mygroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
-- mygroupBy p (x':xs') = go (xs') [[x']]
--   where go [x] rst = rst
--         go (x:y:zs) (acc:accs)
--              | p x y = go (y:zs) ((a:acc):accs)
--              | otherwise = go (y:zs) ([]:(x:acc):accs)


allLongestStrings xs = last $ libgroupBy (\x y -> length x == length y) $ sortBy (comparing length) xs

-- strange behavior of standard groupBy. why not split at 2?
-- *Main> groupBy (<) [1,2,3,2,3,4]
--[[1,2,3,2,3,4]]

-- more intuitive groupBy with libgroupBy
-- *Main> libgroupBy (<) [1,2,3,1,2,3]
-- [[1,2,3],[1,2,3]]



commonCharacterCount s1 s2 = sum $ map snd $ M.toList $ M.intersectionWith min mA mB 
  where mA = count s1
        mB = count s2   


commonCharacterCount' sA sB = sum $ map (uncurry min) $ (\x -> (count x sA, count x sB)) <$> ['a'..'z']
  where count s xs = length $ filter (==s) xs  

-- https://stackoverflow.com/questions/44558242/picture-how-mapaccumr-works
coolPrint = let (_, result) =
                      mapAccumR 
                        (\cumulativeLength item -> 
                          let newLength = cumulativeLength + length item 
                          in (newLength, take (cumulativeLength) (repeat ' ') ++ item ++" ")
                        )
                        0
                        ["Geese", "Monkeys", "Chocolate", "Chips", "Dust", "Box"]
                    in mapM_ putStrLn $ reverse result
-- Box
--    Dust
--        Chips
--             Chocolate
--                      Monkeys
--                             Geese

isLucky n =  (sum frst == sum lst)
    where toList = map digitToInt $ show n
          frst = take ((length toList)`div`2) toList
          lst = drop ((length toList)`div`2) toList


digits' 0 = []
digits' n = digits' (n `div` 10) ++ [n `mod` 10]


-- https://discord.com/channels/280033776820813825/505367988166197268/816533687323328512

-- [1,2,3,4] should give
-- [1]
-- [2]
-- [3]
-- [4]
-- [1,2]
-- [2,3]
-- [3,4]
-- [1,2,3]
-- [2,3,4]

gauravsinghSubLists lst = reverse $ go 1 [] 
  where go cnt acc 
            | cnt == length lst = acc
            | otherwise = go (cnt + 1) ( divvy cnt 1 lst : acc)


cassesgreatidea =  [take 1, take 2, take 3] <*> tails [1,2,3,4]

chal1 l = take <$> [1..pred $ length l] <*> tails l


lc = [-1, 150, 190, 170, -1, -1, 160, 180]

practiceMapAccumA xs = mapAccumR (\a b -> (a+1,(a,b*2))) 0 xs
-- (10,[(9,2),(8,4),(7,6),(6,8),(5,10),(4,12),(3,14),(2,16),(1,18),(0,20)])
practiceMapAccumB xs = mapAccumL (\a b -> (a+1, (a,b*2))) 0 xs
-- (10,[(0,2),(1,4),(2,6),(3,8),(4,10),(5,12),(6,14),(7,16),(8,18),(9,20)])

-- sortByHeight xs = mapAccumL a -> b -> (a, c) a t b
-- 
sortByHeight xs = putBack xs srtPeople
    where srtPeople = sort $ filter (/=(-1)) xs
          putBack [] _ = []
          putBack xs [] = xs
          putBack (x:xs) (p:ppl)
            | x == (-1) = x : putBack xs (p:ppl)
            | otherwise = p : putBack xs ppl

sortByHeight xs = snd $ mapAccumL treesAndPeople sortPeople xs
      where treesAndPeople [] x = ([],x)
            treesAndPeople (p:people) x 
                | x == (-1) = (p:people, x)
                | otherwise = (people, p)
            sortPeople = sort $ filter (/=(-1)) xs 


data Mylist a = RevMe a | Stay a | Cntr [ Mylist a] deriving (Show, Ord, Eq)

ts1 = RevMe "bar"
ts2 = Cntr [Stay "foo", Cntr [RevMe "bar"], Stay "baz"]
-- "foo(bar)baz(blim)"
ts3 = Cntr [Stay "foo", Cntr [RevMe "bar"], Stay "baz", Cntr [RevMe "Blim"]]
mylistrev (RevMe x) = RevMe (reverse x) 
mylistrev (Stay x) = Stay x 
mylistrev (Cntr xs) = Cntr ( map mylistrev xs)  
-- Cntr [Stay "foo",Cntr [RevMe "rab"],Stay "baz",Cntr [RevMe "milB"]]
-- *Main> mylistrev ts1
-- RevMe "rab"
-- *Main> mylistrev ts2
-- Cntr [Stay "foo",Cntr [RevMe "rab"],Stay "baz"]
-- *Main> mylistrev ts3
-- Cntr [Stay "foo",Cntr [RevMe "rab"],Stay "baz",Cntr [RevMe "milB"]]

-- reverseInParentheses xs = go xs 
--   where go [] = []
--         go (x:xs) 
--             | x == '(' =

-- reverseInParentheses xs = go xs []
--   where go (x:xs) stck 
--           | x == '(' = go (dropWhile isAlpha xs) (takeWhile isAlpha xs)
--           | x == ')' =


--  foo(bar(baz))blim
-- "foo(bar)baz(blim)"
reverseInParentheses' xs = head $ foldr f [""] xs
    where f ')' xs = "":xs
          f '(' (x:y:xs) = (reverse x ++ y) : xs  
          f c grp@(x:xs) = trace (show grp) (c:x):xs

reverseInParenthesesR' xs = foldr f [""] xs 
  where f ')' xs = "":xs 
        f '(' (x:y:xs) = (reverse x ++ y) : xs 
        f  c  (x:xs) = (c:x):xs


reverseInParentheses'' inputString = reverse res
    where  (res,_) = foldl revAcc ([],[]) inputString
         
revAcc (acc@(cur,stack)) c  
    | c == '(' = (cur, []:stack) -- start adding to stack
    | c == ')' = -- remove from stack 
                 case stack of 
                     -- TODO may need some extra reverses in here...
                     h:[] -> ((reverse h ++ cur), [])
                     h:h2:tl -> (cur, (reverse h ++ h2):tl)
    | otherwise = case stack of 
                      h:tl -> (cur,(c:h):tl)
                      []   -> (c:cur, stack)
          

reverseInParentheses''' :: String -> String
reverseInParentheses''' s = rev s [] where
    rev [] stk = reverse stk
    rev (')':t) stk = let (s1, s2) = span (/='(') stk in rev t (reverse s1 ++ tail s2)
    rev (h:t) stk = rev t (h:stk)


reverseInParentheses'''' xs =
    case elemIndices '(' xs of
        [] -> xs
        is -> let i = last is 
                  (as,_:bs) = splitAt i xs
                  Just j = elemIndex ')' bs
                  (cs,_:ds) = splitAt j bs
              in reverseInParentheses'''' (as ++ reverse cs ++ ds)


ld = [50, 60, 60, 45, 70]
alternatingSums xs = (\(a , b) -> [tidyUp a, tidyUp b]) $ partition (even.fst) $ zip [0..] xs
  where tidyUp ys = sum  $ map snd ys

alternatingSums' a = f (partition (even . fst) (zip [0..] a))
  where
    f (even,odd) = [sum $ snd (unzip even), sum $ snd (unzip odd)]

alternatingNumbers = transpose [[1,2,3],[4,5,6]]

-- alternatingSums zs = go zs [] []
--       where go [] aa bb = [sum aa, sum bb]
--             go (x:y:zs) aa bb = go (zs) (x:aa) (y:bb)
--             go (x:zs) aa bb = go [] (x:aa) bb


example wholegroup@(x:y:xs) = even $ length wholegroup 

vowelDigit :: String -> Bool
vowelDigit "" = True
vowelDigit(x:y:xs) 
                 | even ( length xs + 2 ) && x `elem` ['A','E','I','O','U','a','e','i','o','u'] && y `elem` ['0'..'9'] = vowelDigit xs 
                 |otherwise = False 


isVowel x = x `elem` ['a','e','i','o','u']
vowelDigit' xs = even (length xs) && all (\x -> isDigit x || isVowel x) (map toLower xs)


picture = ["abc",
           "ded"]

addBorder mtr = topNBottom : map (\x -> '*' : x ++ "*" ) mtr ++ [topNBottom]
    where topNBottom = replicate ((length $ head mtr) + 2) '*'




toBarcode "" = Nothing
toBarcode xs = if all valid convert then Just (convert) else Nothing
  where f '0' = '.'
        f '1' = '|'
        f _ = ' '
        valid x = x `elem` "|."
        convert = (map f xs)

toBarcode' xs = sequence $ map f xs
  where f '0' = Just '.'
        f '1' = Just '|'
        f _ = Nothing



a = [1, 2, 3]
b = [2, 1, 3]

areSimilar as bs = sort as == sort bs && limitMismathes
  where limitMismathes = (<=2) . length . filter (uncurry (/=)) $ zip as bs 

arrayChange :: (Ord a, Num a) => [a] -> a
arrayChange xs = go xs 0  
  where go [x] c = c 
        go (x:y:zs) c 
            | x < y = go (y:zs) c 
            | otherwise = go ((y+calc):zs) (c+calc)
              where calc = (x - y) + 1


arrayChange' xs = head $ foldl ko [0, minBound] xs
  where ko [total, pre] x = if pre >= x then [total + pre - x + 1, pre + 1] else [total, x]


-- bfrct :: [a] -> [Bool] -> ([(a, Bool)], [(a, Bool)])
bfrct xs bls = partition snd $ zip xs bls

rvrsInt :: Int -> Int
rvrsInt num = read $ reverse $ show num


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys


msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort (firstHalf xs)) (msort (secondHalf xs))

firstHalf  xs = let { n = length xs } in take (div n 2) xs
secondHalf xs = let { n = length xs } in drop (div n 2) xs


palindromeRearranging xs = 1 >= (length . filter odd $ map length . group $ sort xs) 

arrayMaximalAdjacentDifference xs = foldr (\(x,y) acc -> max (abs(x-y)) acc) 0 $ zip xs (tail xs)
arrayMaximalAdjacentDifference' xs = maximum . map abs $ zipWith (-) xs (tail xs)

ia = [5, 3, 6, 7, 9]
avoidObstacles xs = maximum $ map abs $ zipWith (-) valid (tail valid)
  where mx = maximum xs
        steps = [2..mx+1] \\ xs
        valid = filter (\stp -> any (\o -> (o `div` stp == 0)) xs ) steps  



image = [[7, 4, 0, 1], 
         [5, 6, 2, 2], 
         [6, 10, 7, 8], 
         [1, 4, 2, 0]]

boxBlur xss = transpose $ map ( map (`div` 9) . threes) . transpose $ map threes xss

threes (x:y:z:xs) = sum [x,y,z] : threes (y:z:xs)
threes _ = []

matrixA = [[True, False, False],
          [False, True, False],
          [False, False, False]]
matrixB = [[False, False, False],
          [False, True, False],
          [False, False, False]]

-- zipWith (zipWith (-)) hrz cnvrtd
minesweeper xss = (hrz , cnvrtd) 
            where sngleRow xs = zipWith3 (\a b c -> a + b + c) (0:xs) xs (tail $ xs++[0]) 
                  cnvrtd = map (map fromEnum) xss
                  hrz = transpose $ map sngleRow . transpose $ map sngleRow cnvrtd



rowA = [1,0,0]

newRow = [2,2,1]


minesweeper' xss = zipWith (zipWith (-)) newGrid cnvrtd
    where cnvrtd = map (map fromEnum) xss
          sngle xs = zipWith3 (\a b c -> a+b+c) (0:xs) xs (tail xs ++ [0])
          newGrid = transpose $ map sngle $ transpose $ map sngle cnvrtd

arrayReplace xs a b = map (\x -> if x == a then b else x) xs

evenDigitsOnly n = all even $ map digitToInt $ show n

-- [1,0,0]
-- 0 1 0 

alphabeticShift :: String -> String
alphabeticShift str = map (\x -> toEnum $ ( ((fromEnum  x) - 96) `mod` 26) + 97 ) str


alphaShift str = map nxt str 
  where nxt x = if x == 'z' then 'a' else succ x 


chessBoardCellColor a b = (==1) $ length . group $ map even [color a, color b] 
  where color sq = sum $ map fromEnum sq 


aa =  ["ab", "bb", "aa"]

bb = ["aba", "bbb", "bab"]

cc = ["abc", 
 "abx", 
 "axx", 
 "abc"]

stringsRearrangement xs = any handleOnePerm $ permutations xs 
handleOnePerm perm = all (==1) $ map (uncurry differences) $ zip perm (tail perm)
differences a b =  length $ filter (==False) $ zipWith (==) a b


arrayMaxConsecutiveSum xs k = maximum $ map sum $ divvy k 1 xs 
arrayMaxConsecutiveSum' xs k = maximum $ map sum $ divvy k 1 xs 

mapDvy xs k = go xs 1 0 (M.empty) []
  where go [x] _ _ myMap _ = myMap
        go (x:xs) smalCnt bgCnt myMap fillList
          | k `mod` smalCnt == 0 = go (x:xs) (1) (bgCnt + 1) (M.insert bgCnt ( fillList) myMap) []
          | otherwise = go xs (smalCnt + 1) (bgCnt) myMap (x:fillList)  
