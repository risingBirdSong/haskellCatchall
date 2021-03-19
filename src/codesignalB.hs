import Data.List
import Data.List.Split
import Data.Char
import Data.Function
import Data.Ord
import Debug.Trace
import Control.Monad

import Data.Maybe

buildPalindrome xs =  concat $ concatMap (nub) $ group $ (group xs) ++ (group $  reverse xs)
buildPalindrome' xs =  (group xs) ++ (group $  reverse xs)

buildPalindromenaive xs = go xs [] []
  where go (x:xs) not palis
          | (x:xs) == reverse (x:xs) = (not, xs:palis)
          | otherwise = go xs (x:not) palis 


isMAC48Address str = ((and $ concat [(concat validchars)]) && validlast && notrailing)
    where prepare =  split (condense $ oneOf "-") str
          chunked =  chunksOf 2 prepare
          firstbit = take 5 chunked
          charlogic = (`elem`  (['0'..'9'] ++ ['A'..'F']) )
          validchars = map ((map charlogic).head) firstbit 
          validlast = (all charlogic $ head $ last  chunked) 
          notrailing = not . null $ head $ last chunked

          checkhyphens = map ((=="-").last) firstbit
isMAC48Address' str =  concat $ splitOn "-" str

lineEncoding str = concatMap lgc $ group str
  where lgc [xs] = [xs]
        lgc s = (show $ length s) ++ [head s] 
          


board = concatMap (\n -> zipWith (\a b -> (fromEnum a) + (fromEnum b)) (['A'..'Z']) (repeat n)) [1..8] 


deleteDigit n = stichBackDeleted $ deleter $ numstoList n 
numstoList n = map digitToInt $ show n 
listtoint ns =  read(map (intToDigit) ns) :: Int
        
deleter ns = filter ((/= (length ns)).length) $ snd $ mapAccumL (\acc list -> (acc+1, delete acc list)) 0 (replicate (length ns + 1) ns)
    where zipped = zip [0..] ns

stichBackDeleted xss = maximum $ map (listtoint) xss


-- longestWord txt = maximumBy (comparing length) $ map (filter (isAlpha)) $ words txt


longestWord txt = maximumBy (comparing length) $ splitWhen (not.isAlpha) txt


ex1 = 1234
ex2 = 1678
-- [1000 , 200 , 30 , 4 todo]
-- 1000

rounders n
    | n < 10 = n 
    | otherwise = 10 * (rounders ((n + n `mod` 10) `div` 10))


candles candlesNumber makeNew = go candlesNumber 0 0
  where
    go 0 _ burned = burned
    go cnd stubs burned = (let new = cnd + stubs in go (new `div` makeNew) (new `mod` makeNew) (burned + cnd))


candles' candles makeNew = go candles 0 0
      where go 0    _ acc = acc
            go cnds lftv acc = (let new = cnds + lftv in go (new `div` makeNew) (new `mod` makeNew) (acc + cnds) )
            

countBlackCells n m = (n^2) + (m^2)

arrayReplace inp rplc sbst = map logic inp 
      where logic x = if x == rplc then sbst else x 


-- For s = "4[ab]", the output should be
-- decodeString(s) = "abababab";

-- For s = "2[b3[a]]", the output should be
-- decodeString(s) = "baaabaaa";

-- For s = "z1[y]zzz2[abc]", the output should be
-- decodeString(s) = "zyzzzabcabc".

-- decodeString s = 

-- resursetasks 

-- For a = [6, 7, 3, 8], the output should be
-- nextLarger(a) = [7, 8, 8, -1]

h =  [6, 7, 3, 8]
ho = [7,8,8,-1]

j = [1, 3, 2, 4]
jo =[3,4,4,-1]
k = [4,5,2,25]






nextLargerRe xs = fst $ foldr logic ([],[]) xs 
  where logic x (rest, stk)
          | null stk = ((-1):rest,[x])
          | x < head stk = ((head stk) : rest, x : stk)
          | otherwise = logic x (rest, tail stk)






nextLarger  xs = fst $ foldr add ([], []) xs
    where
    add x (rest, stack)
        | null stack = (-1 : rest, [x])
        | x < head stack = (head stack : rest, x : stack)
        | otherwise = add x (rest, tail stack)
            
-- input [6,7,3,8]
--  8  [-1]  [8]
--  3  [8,-1] [3,8]
--  7  [8,-1] [8]
--  7  [8,8,-1] [7,8]
--  6  [7,8,8,-1] [6,7,8]
 


climbingStaircase 0 k = [[]]
climbingStaircase n k =  concat $ [map (i:) (climbingStaircase (n-i) k)|i<-[1..opts]]
 where 
  opts = min n k

understanding 0 = [[]]
understanding stairs = concat $ [ map (i:) (understanding (stairs - 1)) | i <- [1..stairs]]

-- [[1,1,1],[1,2,1],[2,1,1],[2,2,1],[3,1,1],[3,2,1]]



-- given n, "queens n" solves the n-queens problem, returning a list of all the
-- safe arrangements. each solution is a list of the columns where the queens are
-- located for each row


  -- foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
  -- foldM folds (from left to right) in the list monad, which is convenient for 
  -- "nondeterminstically" finding "all possible solutions" of something. the 
  -- initial value [] corresponds to the only safe arrangement of queens in 0 rows
 
  -- given a safe arrangement y of queens in the first i rows, and a list of 
  -- possible choices, "oneMoreQueen y _" returns a list of all the safe 
  -- arrangements of queens in the first (i+1) rows along with remaining choices 

    -- "safe x" tests whether a queen at column x is safe from previous queens
-- prints what the board looks like for a solution; with an extra newline

-- queens :: Int -> [[Int]]
queens n = map fst $ foldM oneMoreQueen ([],[1..n]) [1..n]  where 
  oneMoreQueen (y,d) _ = [(x:y, delete x d) | x <- d, safe x]  where
    safe x = and [x /= c + n && x /= c - n | (n,c) <- zip [1..] y]
 
printSolution y = do
     let n = length y
     mapM_ (\x -> putStrLn [if z == x then 'Q' else '.' | z <- [1..n]]) y
     putStrLn ""
 
main = do
  n <- readLn :: IO Int
  mapM_ printSolution $ queens n


mainA = do
  n <- readLn :: IO Int
  mapM_ printSolution $ queens' n

 
queens' :: Int -> [[Int]]
queens' n = foldM f [] [1..n]
    where
      f qs _ = [q:qs | q <- [1..n] \\ qs, q `notDiag` qs]
      q `notDiag` qs = and [abs (q - qi) /= i | (qi,i) <- qs `zip` [1..]]



assembleQueen qs n= [q:qs | q <- [1..n] \\ qs, q `notDiag` qs]
notDiag q qs = and [abs (q-qi) /= i | (qi,i) <- qs `zip` [1..]]

firstPossible = [4,2,7,3,6,8,5,1]


casQueensDebug n = head $ foldM nextQueen [] [1..n] where
  nextQueen qns _ = do
    cur <- [1..n]
    guard $ (cur `notElem` qns) -- No same column
    -- Remove diagonal catches
    guard $ trace (" cur -> " ++ show cur ++ " qns -> " ++ (show qns)) (cur `notElem` zipWith (+) qns [1..])
    guard $ (cur `notElem` zipWith (-) qns [1..])
    return $ cur : qns

casQueens n = foldM nextQueen [] [1..n] where
  nextQueen qns _ = do
    cur <- [1..n]
    guard $ (cur `notElem` qns) -- No same column
    -- Remove diagonal catches
    guard $ (cur `notElem` zipWith (+) qns [1..])
    guard $ (cur `notElem` zipWith (-) qns [1..])
    return $ cur : qns

-- sumSubsets arr num = foldM getsum [] arr 
--       where getsum acc c = do 
--             toaddlist <- acc 
--             toadd     <- toaddlist
--             if ((toadd + c) < num) then return (c:toadd:acc) else return acc
            


-- [4,2,7,3,6,8,5,1]

-- 12345678

-- Q2345678
-- 12Q45678
-- 1234Q678
-- 1Q345678
-- x.xQxxx.
-- the rest of this sequence arent valid

--  cur -> 1 qns -> []
--  cur -> 2 qns -> [1]
--  cur -> 3 qns -> [1]
--  cur -> 2 qns -> [3,1]
--  cur -> 4 qns -> [3,1]
--  cur -> 5 qns -> [3,1]
--  cur -> 2 qns -> [5,3,1]
--  cur -> 4 qns -> [2,5,3,1]
--  cur -> 6 qns -> [4,2,5,3,1]
--  cur -> 7 qns -> [4,2,5,3,1]
--  cur -> 8 qns -> [4,2,5,3,1]


-- 1234Q678
-- 12345678

nogos cur qns = ("pos:", rawZip cur qns, "neg:" , rawZipNeg cur qns)
rawZip :: (Num c, Enum c) => p -> [c] -> [c]
rawZip cur qns = zipWith (+) qns [1..]
rawZipNeg cur qns = zipWith (-) qns [1..]
guardQueenPos cur qns = (cur `notElem` zipWith (+) qns [1..])
guardQueenNeg cur qns = (cur `notElem` zipWith (-) qns [1..])


myGuard = do 
  cur <- [1..10]
  guard $ cur < 5
  return cur


mainCas = do
  n <- readLn :: IO Int
  mapM_ printSolution $ casQueens n

-- ...Q....
-- .Q......
-- ......Q.
-- ..Q.....
-- .....Q..
-- .......Q
-- ....Q...
-- Q.......


remakeclimbing 0 k = [[]]
remakeclimbing stairs steps = concat $ [ map (val:) (remakeclimbing (stairs - val) (steps) ) | val <- [1..lower]]
  where lower = min stairs steps