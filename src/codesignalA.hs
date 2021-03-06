-- {-# LANGUAGE FlexibleContexts #-}

import Data.List
import Data.Bits (xor)
import Numeric 
import Data.Char 
import qualified Data.Map as M
import qualified Data.Vector as V 
import qualified Data.Set as S 
import Control.Lens 
import Data.Maybe
import Data.List.Split
import Debug.Trace
import Control.Monad
import Data.Function
import Data.Ord
import Data.Maybe

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

bigExample = [[ 1, 2, 3, 4,5], 
              [16,17,18,19,6], 
              [15,24,25,20,7], 
              [14,23,22,21,8], 
              [13,12,11,10,9]]

-- 1 == hrz right
-- 2 == vrt down
--3 == hrz left 
-- 4 == vrt up 

--alternating indexs horizontal and vertical
-- h0 -> v4 -> h4r -> v0r -> h1 -> v3 -> h3r -> v1r -> h2  

--horizontals                       verticals
-- h0 -> h4r -> h1 -> h3r -> h2     v4 -> v0r -> v3 -> v1r

hrzIdxs n = take ((n `div` 2) +1) $ zip ([0..n]) (reverse [0..n])
-- [(0,4),(1,3),(2,2)]
vrtcIdxs n = map tup2Rvr $ hrzIdxs n  
-- [(4,0),(3,1),(2,2)]

spiralIdxs n = zip (hrzIdxs n) (vrtcIdxs n)
-- [((0,4),(4,0)),((1,3),(3,1)),((2,2),(2,2))]
tup2Rvr (x,y) = (y,x)

-- spiral :: [[Integer]] -> [[Integer]]
spiral mtrx = (\(hrz,vrt,cnt,acc) -> trace (show acc) acc) $ output
      where hrzMtr = mtrx 
            vrtcMtrx = transpose mtrx
            n = (length mtrx) - 1 
            sprlIndxs = spiralIdxs n
            output = foldr (\((t,r),(b,l)) (hrz,vrt,cnt,ac) -> spiralTurns ((t,r),(b,l)) (hrz,vrt,cnt,ac) ) (hrzMtr, vrtcMtrx, 0,[]) sprlIndxs

spiralTurns ((t,r),(b,l)) (hrz,vrt,cnt,ac)
  | cnt < 1 = trace ("count>" ++ show cnt) (hrz,vrt,cnt+1,(( hrz !! t) : (vrt !! r) : (reverse $ hrz !! b) : (reverse $ vrt !! l) : ac)) 
  -- | cnt < 1 = trace ("count>" ++ show cnt) (hrz,vrt,cnt+1,(( hrz !! t) : (drop 1 $ vrt !! r) : (reverse $ init $ reverse $ hrz !! b) : (reverse $ init $ reverse $ vrt !! l) : ac)) 
  | otherwise = trace ("count>" ++ show cnt) (hrz,vrt,cnt+1,((init $ tail $ hrz !! t) : (init $ tail $ vrt !! r) : (init $ tail $ hrz !! b) : (init $ tail $ vrt !! l) : ac)) 
  -- | otherwise = (hrz,vrt,cnt,ac)

-- (hrz,vrt,cnt+1,(( hrz !! t) : (drop 1 $ vrt !! r) : (init $ hrz !! b) : (init $ vrt !! l) : ac))
-- output = map (\((tx,ry) , (bx,ly)) -> trace ("tx>" ++ (show (mtrx !! tx)) ++ "y>" ++ (show (vrtcMtrx !! ry)) ++ "bx>" ++ (show $ reverse (mtrx !! bx)) ++ "ly>" ++ (show (reverse $ vrtcMtrx !! ly)) ) ((tx,ry) , (bx,ly)) ) $ init sprlIndxs 


spiralNumbers' n = solve (n, n) 1
solve (1, 1) x = [[x]]
solve (r, c) x = [x..x + c - 1] : logic
  where logic = (map reverse . transpose $ solve (c, r - 1) (x + c))

--  [[1, 2, 3, 4,5], 
--  [16,17,18,19,6], 
--  [15,24,25,20,7], 
--  [14,23,22,21,8], 
--  [13,12,11,10,9]]


-- , r> 5 c> 4
--  r> 4 c> 4
--  r> 4 c> 3
--  r> 3 c> 3
--  r> 3 c> 2
--  r> 2 c> 2
--  r> 2 c> 1

spiralNumbers n = spiral n n 1 where
    spiral _ 0 _ = []
    spiral 0 h _ = take h $ repeat []
    spiral w h i = take w [i..] : (transpose $ reverse $ spiral (h-1) w (i+w))



diagsB mtrx = map concat . transpose 
  $ zipWith (\blnks dta -> blnks ++ (map (:[]) dta)) (iterate ([]:) []) mtrx


-- sudukoValid mtrx 



sudgrid' = [['1', '3', '2', '5', '4', '6', '9', '8', '7'],
          ['4', '6', '5', '8', '7', '9', '3', '2', '1'],
          ['7', '9', '8', '2', '1', '3', '6', '5', '4'],
          ['9', '2', '1', '4', '3', '5', '8', '7', '6'],
          ['3', '5', '4', '7', '6', '8', '2', '1', '9'],
          ['6', '8', '7', '1', '9', '2', '5', '4', '3'],
          ['5', '7', '6', '9', '8', '1', '4', '3', '2'],
          ['2', '4', '3', '6', '5', '7', '1', '9', '8'],
          ['8', '1', '9', '3', '2', '4', '7', '6', '5']]


sudgridTrue =[[1, 3, 2, 5, 4, 6, 9, 8, 7],
          [4, 6, 5, 8, 7, 9, 3, 2, 1],
          [7, 9, 8, 2, 1, 3, 6, 5, 4],
          [9, 2, 1, 4, 3, 5, 8, 7, 6],
          [3, 5, 4, 7, 6, 8, 2, 1, 9],
          [6, 8, 7, 1, 9, 2, 5, 4, 3],
          [5, 7, 6, 9, 8, 1, 4, 3, 2],
          [2, 4, 3, 6, 5, 7, 1, 9, 8],
          [8, 1, 9, 3, 2, 4, 7, 6, 5]]

sudgridFalse =[[1, 3, 1, 5, 4, 6, 9, 8, 7],
              [4, 6, 5, 8, 7, 9, 3, 2, 1],
              [7, 9, 8, 2, 1, 3, 6, 5, 4],
              [9, 2, 1, 4, 3, 5, 8, 7, 6],
              [3, 5, 4, 7, 6, 8, 2, 1, 9],
              [6, 8, 7, 1, 9, 2, 5, 4, 3],
              [5, 7, 6, 9, 8, 1, 4, 3, 2],
              [2, 4, 3, 6, 5, 7, 1, 9, 8],
              [8, 1, 9, 3, 2, 4, 7, 6, 5]]

setAndBack xs = S.toList $ S.fromList xs
noDupes xs = length xs == length (setAndBack xs)

sudoku :: Eq a => [[a]] -> Bool
sudoku mtrx = rows && cols && boxes
    where rows = all noDupes mtrx 
          cols = all noDupes (transpose mtrx)
          boxes = all noDupes (blocks mtrx)
          blocks mtrx =  chunksOf 9 . concat . concatMap transpose $ chunksOf 3 mtrx
          noDupes xs = length xs == length (nub xs)


spiralling [] = []   
spiralling grid = head grid : spiralling (reverse $ transpose (tail grid))   

maxMultiple divisor bound = go bound 
  where go n
          | n <= bound && n `mod` divisor == 0 = n 
          | otherwise = go (n-1)

spirallingGrid =  [[ 1, 2, 3, 4,5], 
                  [16,17,18,19,6], 
                  [15,24,25,20,7], 
                  [14,23,22,21,8], 
                  [13,12,11,10,9]]


circleOfNumbers n firstNumber = (n `div` 2 + firstNumber) `mod` n  
circleOfNumbers' n firstNumber = answer
    where range' = take (n*2) $ cycle $ [1..(n-1)] ++ [0]
          answer =  (drop firstNumber range')  !! ((n `div` 2) - 1)



lateRide n = sum $ map (digitToInt) (show hours ++ show minutes)
      where (hours,minutes) = n `divMod` 60

gg n = n `divMod` 60


-- length $ tail $
phoneCall min1 min2_10 min11 s = length $ takeWhile (<=s) . tail 
  . scanl (+) 0 $ [min1] ++ replicate 9 min2_10 ++ repeat min11


practice = "practice makes perfect. you'll only get Perfect by practice. just practice!"

wordCount ws = lastbit 
  where lastbit = map (\x -> (head x, length x)) $ group . map snd . concatMap (sort . concat) 
          . groupBy (\ a b -> length a == length b) . sortOn (Down . length) 
          $ groupBy (\(_,a) (_, b) -> a == b)  $ sortOn (Down . snd) firstbit
        firstbit = zip [0..] $ words . filter (\x -> isAlpha x || isSpace x) $ map toLower ws 


reachNextLevel exp thrsh rwrd = (exp + rwrd) >= thrsh

-- knapsackLight v1 w1 v2 w2 mx = sum . map fst . takeWhile (\(v,w) -> w <= mx) 
--                . scanl (\(val, ac) (v,w) -> (v,w+ac)) (0,0) 
--                 $ sortOn Down [(v1,w1),(v2,w2)]


knapsackLight v1 w1 v2 w2 mx
  | w1 + w2 <= mx = v1 + v2 
  | w1 <= mx && w2 <= mx = max v1 v2 
  | min w1 w2 > mx = 0
  | otherwise = case min w1 w2 of
                  w1 -> v1 
                  w2 -> v2


extraNumber a b c = head . head . filter ((==1) . length) . group $ sort [a,b,c]
extraNumber' a b c =  a `xor` b `xor` c


isInfiniteProcess a b = go a b
  where go a b 
          | a > b = True 
          | a == b = False 
          | otherwise = go (succ a) (pred b)   

isInfiniteProcess' a b | odd a == odd b = a > b
                      | otherwise = True


isInfiniteProcess'' a b = a > b || ((b - a) `mod` 2) == 1

arithmeticExpression a b c = any (==True) [a+b==c,a-b==c,a*b==c,fractDiv a b c]

fractDiv a b c = (realToFrac a)/(realToFrac b) == (realToFrac c)


arithmeticExpression' a b c = any (\f -> a' `f` b' == c') [(+), (-), (*), (/)]
  where [a', b', c'] = map toRational [a, b, c]


willYou young beautiful loved 
  | not yngBty && loved = True 
  | not loved && yngBty = True 
  | otherwise = False
    where yngBty = young && beautiful 


goodFacts n = scanl (*) 1 [2..n]

fact n = product $ [1..n]

facts stop = go 2 
  where go n
         | n > stop = []
         | otherwise = fact n : go (succ n) 

leastFactorial n = fromMaybe 0 $ find (>=n) $ facts n


leastFactorial' n = head $ dropWhile (<n) facList
    where
        facList = scanl (*) 1 [1..]

leastFactorial'' n = walk 1 1
  where walk m k = if k >= n then k else walk (m+1) (k*m)
        -- the invariant here is that k = (m-1)!

leastFactorial''' n =  head $ filter (>= n) facts
  where facts = scanl1 (*) [1..]

leastFact_remake n =  head $ dropWhile (<n) factList 
    where factList = scanl (*) 1 [2..n]

leastFact_remake' n = walk 1 1 
  where walk ones fct
          | fct > n = fct 
          | otherwise = walk (succ ones) (fct*ones) 

factWalk n = go 1 1
  where go walk fct 
          | fct > n = []
          | otherwise = walk*fct : go (walk+1) (walk*fct)


countSumOfTwoRepresentations2 n l r = [[x,y] | x <- [l..r], y <- [l..r], (x+y) == n, x<=y]
countSumOfTwoRepresentations2' n l r = [[x,n-x] | x <- [l..r],x<=n-x,(n-x)<=r]

-- community solution 
countSumOfTwoRepresentations2'' n l r = min leftBound rightBound where
    leftBound = max 0 $ div n 2 - l + 1
    rightBound = max 0 $ r - div (n+1) 2 + 1


magicalWell a' b' n' = sum $ go a' b' n' 
  where go a b n
          | n == 0 = []
          | otherwise = (a*b) : go (succ a) (succ b) (pred n) 


-- magicalWel' a b n =  foldr (\ (x,y) acc -> (x*y) + acc) 0 $ zip [a..n+a] [b..(n+b)-1]  
      
iteratedwell a b n = sum $ map (uncurry (*)) $ take n $ iterate (\(x,y)->(x+1, y+1)) (a,b)

-- from community, great!
magicalWell' a b n =
    sum $ take n $ zipWith (*) [a..] [b..]
 

lineUp = sum . map (fromEnum . even) . scanl1 (+) . map (fromEnum . (/= 'A'))

lineUp' commands = (sum $ map fromEnum $ scanl (\acc x -> if acc then x == 'A' else x `elem` "LR") True commands) - 1
lineUp'' = pred . length . filter id . scanl (flip ($)) True . map (\c -> if c == 'A' then id else not)
lineUp''' commands = sum [1 | z <- scanl1 (+) $ map ((+1).ord) commands, even z]


centry n =  ((n - 1) `div` 100) + 1

pali str = str == (reverse str)


examp = [3, 6, -2, -5, 7, 3]
adjacentElementsProduct xs = maximum . map (uncurry (*)) $ zip xs (tail xs)


-- thisCurriedFunction a b c = a + b + c 

-- onceCurry = thisCurriedFunction 1
-- twoCurry = onceCurry 2
-- finalCurry = twoCurry 3

-- read (concat solve) :: Int
additionWithoutCarrying param1 param2 = read solve :: Int  
    where s1 = show param1
          s2 = show param2 
          [shrt, lng] = sortBy (comparing length) [s1,s2]
          (paddedA, paddedB) = ((replicate ( length lng - length shrt) '0' ++ shrt),lng)
          solve = zipWith (\a b -> last $ show ((digitToInt a) + (digitToInt b))) paddedA paddedB
          


additionWithoutCarrying' a' b' = reverse $ go a' b' 
  where divten x = x `div` 10
        lastDgt x = x `mod` 10
        go a b 
            | a == 0 && b == 0 = []
            | otherwise = lastDgt (lastDgt a + lastDgt b) : additionWithoutCarrying' (divten a) (divten b)  




additionWithoutCarrying'' a b = go a b 1 
  where go 0 0 _ = 0 
        go x y dgt = (((x `mod` 10 + y `mod` 10) `mod` 10) * dgt) + go (x `div` 10) (y `div` 10) (dgt * 10) 

appleBoxes k = (evens-odds)
    where odds = sum . map (^2) $ filter odd apples
          evens = sum . map (^2) $ filter even apples 
          apples = [1..k] 


appleBoxes' k = foldr (\x r -> x^2 * (-1)^x + r) 0 [1..k]
appleBoxes'' k = sum . map (\x -> if odd x then -x else x ) $ map (\x -> x*x) [1..k]

stripEnd f xs = reverse . dropWhile f $ reverse xs 
numsToList n = map digitToInt $ show n


increaseNumberRoundness n = nonzeros >= 1 && zeros >= 1
    where trailingzs = takeWhile (==0) $ reverse numsToList 
          rest = take (length numsToList - length trailingzs) numsToList
          nonzeros = length $ filter (/=0) rest
          zeros = length $ filter (==0) rest
          numsToList = map digitToInt $ show n

increaseNumberRoundnessAgain nums = any (==0) toList 
    where toList = stripEnd (==0) $ numsToList nums

increaseNumberRoundness' n = 0 `elem` res && any (/= 0) res
    where
        res =  dropWhile (==0) . reverse . map digitToInt . show $ n

increaseNumberRoundness'' n = any (< '1') . dropWhile (< '1') . reverse $ show n


rounders n
    | n < 10 = n
    | otherwise = 10 * rounders (div (n + mod n 10) 10)

rounders_rmk n  
  | n < 10 = n 
  | otherwise  =  10 * rounders ((n + (n `mod` 10)) `div` 10)



candles a b = a + f a b
  where f a b
          | a < b = 0
          | otherwise = trace ("a>" ++ show a) (a `div` b) + f ((a `mod` b)+(a `div` b)) b

candlesRmk cndls mn = cndls + go cndls mn 
  where go candles makeNew 
            | candles < makeNew = 0 
            | otherwise = candles `div` makeNew + go (candles `div` makeNew + candles `mod` makeNew) makeNew


