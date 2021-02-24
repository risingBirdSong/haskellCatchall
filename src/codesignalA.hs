import Data.List
import Numeric 
import Data.Char 
import qualified Data.Map as M
import qualified Data.Vector as V 
import Control.Lens 
import Data.Maybe
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
