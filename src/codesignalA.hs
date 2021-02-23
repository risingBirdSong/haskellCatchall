import Data.List
import Data.Char 
import qualified Data.Map as M
import qualified Data.Vector as V 
import Control.Lens 
import Debug.Trace

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

