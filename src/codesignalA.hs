-- {-# LANGUAGE FlexibleContexts #-}

import Data.List
import Numeric 
import Data.Char 
import qualified Data.Map as M
import qualified Data.Vector as V 
import Control.Lens 
import Data.Maybe
import Data.List.Split
import Debug.Trace
import Control.Monad

foldCount xs = foldr logic (M.empty) xs 
    where logic x mp 
              | M.member x mp = M.adjust (+1) x mp
              | otherwise = M.insert x 1 mp 

names = ["a(1)", 
 "a(6)", 
 "a", 
 "a", 
 "a", 
 "a", 
 "a", 
 "a", 
 "a", 
 "a", 
 "a", 
 "a"]

files = ["doc", "doc", "image", "doc(1)", "doc"]


companyBotStrategy trn =  case (map head $ filter (\[v,crct] -> crct == 1) trn) of 
                                [] -> 0.0
                                xs -> (realToFrac $ sum xs) / (realToFrac $ length xs)

testMatrix = [
  [1, 2, 3],
  [4, 5, 6],
  [7, 8, 9]
  ]

diagsA mtrx = zipWith (\blnks rows -> blnks ++ map (:[]) rows) (iterate ([]:) []) mtrx

-- fileNames fls 
-- fAppnd f n = f ++ "(" ++ show n ++ ")"  

-- ["a(1)", 
--  "a(6)", 
--  "a", 
--  "a", 
--  "a", 
--  "a", 
--  "a", 
--  "a", 
--  "a", 
--  "a", 
--  "a", 
--  "a"]

-- ["a(1)", 
--  "a(6)", 
--  "a", 
--  "a(2)", 
--  "a(3)", 
--  "a(4)", 
--  "a(5)", 
--  "a(7)", 
--  "a(8)", 
--  "a(9)", 
--  "a(10)", 
--  "a(11)"]

-- im thinking of a better
-- way to store multiple encounters in the Map above, rather than store multiple copies of a file which is confusing,
-- store a single file as the key, and its value will be a list of all the dupelicates that have been encountered
-- example above on line 65 with the list of as... store only a single a, with a list of the already taken numbers
-- so follow the iterations ->
-- (a: [1]) -> (a: [1,6]) -> (a : [0,1,6]) -> (a : [0,1,2,6]) ... etc
-- notice that the third a gets output as simple "a" because zero wasnt taken yet.

-- below is a helper function to find the next occurence
nextNumber taken = go (sort taken) [0..]
  where go [] (n:ns) = (n, (sort (n:taken)))
        go (t:tkn) (n:ns) 
          | n < t = (n, sort (n:taken))
          | n == t = go tkn [(n+1)..]


fileNames files = go files (M.empty) 
  where go  []  mp = []
        go (f:fs) mp 
          | M.member (parseFileForLtr f) mp =  (fAppnd f currentNumber) : go fs (M.insert (parseFileForLtr f) (nextNums) mp)
          | otherwise = f : go fs (M.insert (parseFileForLtr f) ([parseFileForNum f]) mp)
            where fAppnd f n = f ++ "(" ++ show n ++ ")" 
                  getFromM k = case (M.lookup k mp) of 
                                Just x -> x
                  (currentNumber,nextNums) = nextNumber $ getFromM (parseFileForLtr f)


parseFileForNum f' 
  | last f' == ')' = digitToInt $ last $ init f'
  | otherwise = 0

parseFileForLtr f 
  | last f == ')' =  reverse $ drop 1 $ dropWhile (isNumber) $ (reverse $ init f)
  | otherwise = f
-- getFromM k = case (M.lookup k mp) of 
--                                 Just x -> x

mymp = M.fromList [('a', [1,6])]


fnames = ["doc", 
        "doc", 
        "image", 
        "doc(1)", 
        "doc"]

theseas = ["a(1)", 
   "a(6)", 
   "a", 
   "a", 
   "a", 
   "a", 
   "a", 
   "a", 
   "a", 
   "a", 
   "a", 
   "a"]


fileNaming' names = reverse $ foldl' addName [] names
    where addName acc name = (if name `elem` acc then getNewName name acc else name) : acc
    
getNewName name nameList = head $ dropWhile (\x -> elem x nameList) newNames
    where newNames = map (\n -> concat [name,"(",show n,")"]) [1..]
    

fileNaming'' xs = f [] xs

f xs [] = []
f xs (y:ys) =
    z : f (z:xs) ys
    where
    z = if elem y xs then g y xs else y

g x xs =
    head $ filter (\e -> not $ elem e xs) $ map (\i -> x ++ "(" ++ show i ++ ")") [1..]

fileNaming''' names = foldl' go [] names
    where go acc name = case elem name acc of
                            False -> acc ++ [name]
                            True  -> acc ++ [makeUnique name acc]

makeUnique name names = concat [name, "(" ,modifier, ")"]
    where modifier = show $ 
                     fromJust $ 
                     find (\x -> not $ elem (concat [name, "(", show x, ")"]) names) [1..]

findSmallestUnused s m = case M.lookup s m of Nothing -> s
                                              Just True -> let tries = zipWith (\n s -> s ++ "("++(show n)++")") [1..] (repeat s)
                                                    in fromJust $ find (\s -> M.notMember s m) tries

tries s = take 10 $ zipWith (\n s -> s ++ "("++(show n)++")") [1..] (repeat s)

fileNaming'''' a = go M.empty a 
           where go m [] = []
                 go m (s:ss) = let s' = findSmallestUnused s m 
                        in s' : go (M.insert s' True m) ss


numToBin x = showIntAtBase 2 intToDigit x ""
-- "101" 

binToNum bins = sum $ reverse $ go (reverse $ bins) 1
  where go [] mlt = []
        go (b:bs) mlt = ((digitToInt b)*mlt) : go bs (mlt * 2)


somecode =  "010010000110010101101100011011000110111100100001"

-- "111"
-- (0 + 1) -> (2 + 1) -> (6 + 1) == 7
-- "101" 
-- (0+1) -> (2 + 0) -> (4 + 1) == 5  


toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
messageFromBinaryCode code = map chr $ map toDec $ chunksOf 8 code  




cryptsolution = [['O', '0'],
                  ['M', '1'],
                  ['Y', '2'],
                  ['E', '5'],
                  ['N', '6'],
                  ['D', '7'],
                  ['R', '8'],
                  ['S', '9']]

anothersolution = [['O', '1'],
                ['T', '0'],
                ['W', '9'],
                ['E', '5'],
                ['N', '4']]


convertToMap :: (Foldable t, Ord a) => t [a] -> M.Map a a
convertToMap = foldr (\[l,n] acc -> M.insert l n acc) M.empty  

isCryptSolution xs slnt = (x + y == z) && (and $ map noleading $ mapped) 
  where mp = convertToMap slnt
        logic x = case (M.lookup (x) mp) of
                        Just x -> x
        noleading (x:xs) 
            | x == '0' && (not.null) xs = False  
            | otherwise = True
        mapped = map (map logic) xs
        [x,y,z] = map (\x -> read x :: Int) mapped

mtrx =[[1,2,3],
      [4,5,6],
      [7,8,9]]

--  [[1, 2, 3],
--   [8, 9, 4],
--   [7, 6, 5]]

-- [1,2,3] [8,9,4] [7,6,5]

newMtrx = chunksOf 3 [1..9]

-- [[1, 2, 3], [8, 9, 4], [7, 6, 5]]
-- [[1, 2, 3], [4, 9, 8], [5, 6, 7]]

lastSpiral n = [(2*n - 1)..(3*n - 2)]


prttyMtr mtr = mapM_ (print) mtr

-- spiral n = go (n*n) [] [1..(n*n)]
--     where go n' (xs) (v:vs)
--               | n' == 20 = []
--               | otherwise = v : go (n'-1) xs vs
          -- go n (s:structrs) (v:vals) = go (n-1) (v:s:structrs) (vals)          

-- *Main> vertical [[],[],[]] [1,2,3]
-- [[1],[2],[3]]

-- [[1,2,3,4,5], 
--  [16,17,18,19,6], 
--  [15,24,25,20,7], 
--  [14,23,22,21,8], 
--  [13,12,11,10,9]]

bigExample = [[1,2,3,4,5], 
              [16,17,18,19,6], 
              [15,24,25,20,7], 
              [14,23,22,21,8], 
              [13,12,11,10,9]]

-- 1 == hrz right
-- 2 == vrt down
--3 == hrz left 
-- 4 == vrt up 

spiralViewer mtrx = dirctr (mtrx) 1
  where dirctr (xs:xss) dir
          | dir == 1 = trace (show $ hrzRight xs dir) (dirctr ((hrzRight xs dir):xss) 3) 
          | dir == 3 = hrzLeft xs dir
        hrzRight [] dir = [] 
        hrzRight (x:xs) dir = x : hrzRight xs dir 
        hrzLeft [] dir = []
        hrzLeft (x:xs) dir = (hrzLeft xs dir) ++ [x]