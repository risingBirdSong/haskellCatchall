import Numeric
import Data.Char
import qualified Data.Map as M
import Data.List

numsToList nums = map digitToInt $ show nums

numToNumList n = reverse $ go n 
  where go 0 = []
        go num = snd (num `divMod` 10) : go (num `div` 10)

toNum lst = fst $ foldr (\x (acc,place) -> (acc + (place * x) , place * 10) ) (0,1) lst
toListNums strnum = map (\x -> read [x] :: Int ) $ drop 1 $ init $ show strnum 

filterTwo f [] = [] 
filterTwo f [x] = [] 
filterTwo f (x:y:ls)
  | (f x y) = x : y : filterTwo f (y:ls)
  | otherwise = filterTwo f (y:ls)


-- https://stackoverflow.com/questions/22403029/how-to-zip-lists-with-different-length
-- brilliant helper function from the community
-- because sometimes you want to your zip to keep going till both lists are empty
-- so just fill the padding in with mempty ish type values
zipWithPadding :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithPadding a b (x:xs) (y:ys) = (x,y) : zipWithPadding a b xs ys
zipWithPadding a _ []     ys     = zip (repeat a) ys
zipWithPadding _ b xs     []     = zip xs (repeat b)

-- a helper function I wrote wanting to zip when the values are Just, otherwise stop zipping
maybeZip ((Just x):xs) ((Just y):ys) = (Just x, Just y) : maybeZip xs ys
maybeZip _  _ = [] 

count :: (Ord a, Integral b) => [a] -> M.Map a b
count =
  foldr updateMap M.empty
    where updateMap v counts
            | M.member v counts = M.adjust succ v counts
            | otherwise           = M.insert v 1 counts

-- note, this function is already provided in Data.Ordered, but not always available, like it isnt in codesignal
myIsSorted [] = True 
myIsSorted [x] = True 
myIsSorted (x:y:zs) 
      | x < y = myIsSorted (y:zs)
      | otherwise = False     


foldCount xs = foldr logic (M.empty) xs 
    where logic x mp 
              | M.member x mp = M.adjust (+1) x mp
              | otherwise = M.insert x 1 mp 


-- finds the next available number in a sequence, preferring lowest numbers above zero
--with no duplicates
-- this is a helper function coming from this challenge
-- https://app.codesignal.com/arcade/intro/level-12/sqZ9qDTFHXBNrQeLC
nextNumber taken = go taken [0..]
  where go [] (n:ns) = (n, (sort (n:taken)))
        go (t:tkn) (n:ns) 
          | n < t = (n, sort (n:taken))
          | n == t = go tkn [(n+1)..]

-- binary to number conversion, a good way I found
toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- 
numToBin x = showIntAtBase 2 intToDigit x ""


-- im sure better ways to do it, but this concept came up
-- change a single element in a list 
changeOne xs n f = frstPart ++ [(f changed)] ++ lastPart
  where frstPart = take n xs
        lastPart = drop (n+1) xs
        changed = head $ drop n xs 


stripEnd f xs = reverse . dropWhile f $ reverse xs 
