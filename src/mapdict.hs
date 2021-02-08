import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Myb
import Control.Applicative
import qualified Data.List.HT as HT 
-- https://leetcode.com/problems/merge-two-binary-trees/
-- 617. Merge Two Binary Trees

-- mtbs [] [] = []
-- mtbs [] ()

-- root1 = [1,3,2,5], root2 = [2,1,3,null,4,null,7]
a =  [1,3,2,5]
b =  [2,1,3,0,4,0,7]



ma = [Just 1,Just 3,Just 2,Just 5]
mb = [Just 2,Just 1,Just 3, Nothing ,Just 4,Nothing,Just 7]
-- [Just 1,Just 3,Just 2,Just 5]
 
-- [Just 2,Just 1,Just 3,Nothing,Just 4,Nothing,Just 7]
myConvert n 
  | n /= (-1) = Just n 
  | otherwise  = Nothing 


mybZip ((Just x):xs) ((Just y):ys) = Just (x, y) : mybZip xs ys
mybZip _  _ = [] 

apZip :: (Num a) =>  [Maybe a] -> [Maybe a] -> [Maybe (a, a)]
apZip = zipWith (liftA2 (,))


zipWithPadding :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithPadding a b (x:xs) (y:ys) = (x,y) : zipWithPadding a b xs ys
zipWithPadding a _ []     ys     = zip (repeat a) ys
zipWithPadding _ b xs     []     = zip xs (repeat b)

--  HT.takeWhileJust (apZip ma mb)
-- [(1,2),(3,1),(2,3)]