import Test.QuickCheck
import Data.List
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