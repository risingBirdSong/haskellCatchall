import Data.List
import Data.Char
import Data.Bool
import qualified Data.Set as St


automorphic_a :: Integer -> String
automorphic_a n = bool "Not!!" "Automorphic" $ show n `isSuffixOf` show (n^2)

sortNumbers :: [Int] -> Maybe [Int]
sortNumbers [] = Nothing
sortNumbers xs =  Just (sort xs)

automorphic :: Integer -> String
automorphic num 
  | amph num = "Automorphic"
  | otherwise = "Not!!"
amph num = num == end
          where end = digitsToEnd num

getend digits n = toNum (drop ((length digits) - (length (convertBtr n))) digits) 
sqrThenLst n = convertBtr (n*n) 

digitsToEnd num = getend (sqrThenLst num) num

convertSlow 0 = []
convertSlow num =  convertSlow (num `div` 10) ++ [snd (num `divMod` 10)]

convertBtr num = reverse (convertBtr' num)
convertBtr' 0 = []
convertBtr' num = snd (num `divMod` 10) : convertBtr' (num `div` 10)

toNum lst = fst $ foldr (\x (acc,place) -> (acc + (place * x) , place * 10) ) (0,1) lst

sqr n = n * n 

strangeAdd a b = length (concat [ones a , ones b])
              where ones n = take n $ repeat "this is a weird way to add" 

-- ones n = take n $ repeat 1 
-- Minimize Sum Of Array (Array Series #1)
-- minSum :: Num b => [b] -> [b]
minSum :: (Num a, Ord a) => [a] -> a
minSum nums = sum $ take half 
                  $ zipWith (*) (srted) (reverse srted)
                  where srted = sort nums
                        half = (length nums `div` 2)


aa ls = (zip <*> reverse) $ sort ls 

maxNumber :: Int -> Int 
maxNumber n = read $ reverse $ sort $ show n 

paddedNums n = "Value is " ++ (reverse $ take 5 $ reverse $ ("0000" ++ show n))

getSum :: (Num a, Ord a, Enum a) => a -> a -> a
getSum a b = sum [(min a b)..(max a b)]


-- cnsPrs n = not . null $ filter (\(x,y) -> x + y == n) $ zip [1..n] (tail [1..n])
-- consecutiveDucks n = 

cd n = filter (==n) $ zipWith (+) rng (tail rng) 
                  where rng = [1..n+1]

manyanswers n = map cd [1..n]                  

-- hmm why doesn't this work?

counterlong list item = foldl (\acc x -> if item == x then (acc+1) else acc  ) 0 list

counter ls item = length $ filter (== item) ls 

-- stringMatch
-- https://www.codewars.com/kata/59ca8e8e1a68b7de740001f4/train/haskell
-- stringMatch :: Eq a => [a] -> [a] -> [Int]
stringMatch :: Eq a => [a] -> [a] -> [Int]
stringMatch as bs = map (counter as) bs

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)
-- isInt x = x == (round x)

consecutiveDucks :: Int -> Bool
consecutiveDucks x = not $ isInt (logBase 2 (fromIntegral x))

convert x = fromIntegral x :: Float 

geoMetricSequence n = 1 : geoMetricSequence' n 1
geoMetricSequence' cutoff acc
  | cutoff <= 0 = [] 
  | otherwise = (acc * 2) : geoMetricSequence' (cutoff - 1) (acc * 2)


geoMetricSequenceA n = take n ([ 2 ^ x | x<-[0..]])
-- filter (not . consDuck) [1..10]
-- [1.0,2.0,4.0,8.0]

keepH hrs = floor (hrs * 0.5)

litres :: Double -> Integer
litres d = floor (d * 0.5)

-- https://www.codewars.com/kata/5727bb0fe81185ae62000ae3/haskell
-- Backspaces in string
cleanString :: String -> String
cleanString str = reverse $ cleanString' (reverse str) 0
cleanString' [] _ = []
cleanString' (s:ss) del 
  | s == '#' = cleanString' (ss) (del+1)
  | del > 0 = cleanString' (ss) (del-1)
  | otherwise = s : cleanString' (ss) (del)

-- brilliant solution from code wars community!
cleanString_a :: String -> String
cleanString_a = reverse . foldl t ""
  where t cs '#' = drop 1 cs
        t cs  c  = c : cs


-- https://www.codewars.com/kata/5bd776533a7e2720c40000e5
input = reverse [9,4,6,4,10,5]
pendulum xs = pendulum' [] [] (sort xs)
pendulum' left right [] = left ++ reverse right
pendulum' left right [x] = x:left ++ reverse right
pendulum' left right (x:y:ls) = pendulum' (x:left) (y:right) ls 


isHappyYear ys = length ls == length (St.fromList ls ) where ls = show ys

nextHappyYear yr = nextHappyYear' (yr + 1) 
nextHappyYear' yr 
  | isHappyYear yr = yr
  | otherwise = nextHappyYear' (yr + 1)
  


-- start [1,2,3,4,5,6]
-- 1 : rec ls ++ [2]
-- 3 : rec ls ++ [4]
-- 5 : rec ls ++ [6]
--      []
-- outputs
-- [1,3,5,6,4,2]

--  ([6, 6, 8 ,5 ,10]) ==> [10, 6, 5, 6, 8]
--  [5,6,6,8,10] ==> [10, 6, 5, 6, 8]


-- [5,6,6,8,10]
-- 5 
-- 5 6
-- 6 5 6
-- 6 5 6 8
-- 10 6 5 6 8

primes = map head $ scanl (\\) [2..] [[p, p+p..] | p <- primes]
                    
primesTo n = sieve [2..n]
        where sieve (x:xs) = x : sieve (xs \\ [x, x * 2..n]) 
              sieve [] = []
isPrime k = if k > 1 then null [ x | x <- [2..k - 1], k `mod` x == 0] else False


findNextPrime x = findNextPrime' x (length $ primesTo x)
findNextPrime' x orgn
  | length (primesTo x) > orgn = primesTo x
  | otherwise = findNextPrime'(succ x) orgn  


minimumNumber xs 
  | isPrime $ sum xs = 0 
  | otherwise = last (findNextPrime (sum xs)) - sum xs

reverseSeq n = [n,n-1..1] 

maxMultiple :: Int -> Int -> Int
maxMultiple dvby bound = maxMultiple' dvby bound 
maxMultiple' divby cur 
  | snd (cur `divMod` divby) == 0 = cur
  | otherwise = maxMultiple' divby (pred cur)
   

specialNumber :: Int -> String
specialNumber n = f (toListNums n)
 where f [] = "Special!!"     
       f (n:ns)     
         | n `elem` specials = f ns
         | otherwise = "NOT!!"
          where specials = [0,1,2,3,4,5] 

toListNums strnum = map (\x -> read [x] :: Int ) $ show strnum 

specialNum n = if all (<='5') $ show n then "Special!!" else "NOT!!"

getCount :: String -> Int
getCount str = length $ filter (`elem` "aeiou") str 

mult3or5 n = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1..(n-1)]

oddExample = [1,1,2,2,3,3,3,4,4,4,4,3,3]
-- findOdd :: [Int] -> Int
findOdd xs = head $ head $ filter (odd . length) $ groupBy (==) $ sort xs

pangram str = all (`elem` lwrCaseStr) ['a'..'z']
          where lwrCaseStr = map toLower str


digitalRoot :: Integer -> Integer
digitalRoot n = head $ go ( map (\x-> read [x] :: Integer) $ show n)
  where go n   
          | length n == 1 = n
          | otherwise = go ( map (\x-> read [x] :: Integer) $ show (sum n))
