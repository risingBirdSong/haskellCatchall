import Debug.Trace
import Data.List
import Data.Char
import Data.List.Split
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

rankT lst = map (\x -> (+1) <$> (elemIndex x sorted)) lst
    where sorted = sort lst 