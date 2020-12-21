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