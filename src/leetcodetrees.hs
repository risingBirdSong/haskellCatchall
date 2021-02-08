-- https://leetcode.com/problems/merge-two-binary-trees/
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List


zipWithPadding :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithPadding a b (x:xs) (y:ys) = (x,y) : zipWithPadding a b xs ys
zipWithPadding a _ []     ys     = zip (repeat a) ys
zipWithPadding _ b xs     []     = zip xs (repeat b)

a =  [1,3,2,5]
b =  [2,1,3,0,4,0,7]

-- solution 
mrgTwBt as bs = M.fromList . map (\(x:xs) -> (x, length (x:xs))) 
                . group . sort . filter (/= 0) $ map (\x -> uncurry (+) x) 
                 $ zipWithPadding 0 0 as bs  