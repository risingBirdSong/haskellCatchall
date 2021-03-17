import Data.List
import Data.List.Split
import Data.Char
import Data.Function
import Data.Ord
import Debug.Trace

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
 

pairs xs = zip xs (tail xs) 
sorted xs =and [x < y | (x,y) <- pairs xs]

playA k = [ replicate k a | a <- [1..k]]
playB k = replicate k [1..k]

-- climbing = 

-- how can this be down with list comprehension ?
-- climbingStairs stairs steps =  [ lst | lst <- [1..steps] , (sum lst) == stairs]


