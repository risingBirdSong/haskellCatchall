import Test.QuickCheck
import Data.List
import Data.List.Split
import Data.Maybe

myScan f init acc [] = acc 
myScan f init acc (l:ls) = myScan f (f init l) (acc ++ [(f init l)]) ls 

inverseScan f ls = go f [] (reverse ls) 
  where go f acc [] = acc
        go f acc [x] = x : acc
        go f acc (a:b:ls) =  go f (f a (negate b) : acc) (b:ls) 

rngSum ls = myScan (+) 0 [] ls 
invsRngSum ls = inverseScan (+) (rngSum ls) 

runningSumCheck ls = ls == invsRngSum ls     
runningSumTest = verboseCheck (runningSumCheck :: [Int] -> Bool)

-- grtstCandy :: NonEmptyList (NonNegative Integer) -> (NonNegative Integer) -> [Bool]
-- grtstCandy :: (Ord a, Num a) => [a] -> a -> [Bool]
grtstCandy ls e = map (\x -> x + e >= maximum ls) ls 

-- grtstex =  grtstCandy [2,3,5,1,3] 3
-- grtstrsmax :: NonEmptyList ([NonNegative Integer]) -> (NonNegative Integer) -> Bool
grtstrsmax ls e = True == (grtstCandy ls e !! fromMaybe 0 ( elemIndex (maximum ls) ls)) 
-- grtstrsmin :: (Ord a, Num a) => [a] -> a -> Bool
grtstrsmin ls e = False  == (grtstCandy ls e !! fromMaybe 0 ( elemIndex (minimum ls) ls)) 

grtsttest :: (NonEmptyList Integer) -> (NonNegative Integer) -> Property 
-- grtsttest :: (NonEmptyList (NonNegative Integer)) -> (NonNegative Integer) -> Property 
-- grtsttest :: (Ord a, Num a) => NonEmptyList a -> NonNegative a -> Property
grtsttest (NonEmpty ls) (NonNegative e) = (minimum ls + e < maximum ls)  ==> (grtstrsmax ls e) == True && (grtstrsmin ls e ) == True 
grtstcheck = verboseCheck (grtsttest)

-- 1470. Shuffle the Array

myshuffle ls = concat $ lstZip xs zs 
        where (xs, zs) = splitAt n ls
              n = (length ls) `div` 2

myunshuffle ls = uncurry (++) $ lstUnzip $ chunksOf 2 ls

shuffletest ls = even (length ls) ==> myunshuffle (myshuffle ls) == ls
shufflecheck = quickCheck (shuffletest :: [Integer] -> Property)

lstZip xs [] = [] 
lstZip [] ys = []
lstZip (x:xs) (y:ys) = [x,y] : lstZip xs ys

lstUnzip ls = (\(xs,ys) -> (reverse xs, reverse ys)) $ go ls ([],[]) 
  where go [] acc = acc 
        go ([x,y]:ls) (l,r) = go ls (x:l,y:r)