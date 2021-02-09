import Data.List 
import Data.List.Split
import Debug.Trace
clmbA =  [1 , 100, 1, 1, 1, 100, 1, 1, 100, 1]

climbing [] steps = []
climbing [cur] steps = [cur]
climbing (cur:[x]) steps = [cur]
climbing (cur:x:y:zs) steps 
  | x < y =  cur : climbing (x:y:zs) (steps + 1)
  | x >= y = cur : climbing (y:zs) (steps + 1)
climbSolve (path) = minimum $ map (sum) [(climbing (drop 1 path) 0) , (climbing path 0)] 


climbing' xs = sum . map minimum $ chunksOf 2 xs
climbingSolve' xs = min (climbing' xs) (climbing' (drop 1 xs))

-- x 

-- trace (min x y + a)
-- [1 , 100, 1, 1, 1, 100, 1, 1, 100, 1]
climbSolveCoffee (x:y:xs) = go x y xs
  where
    go x y [] = min x y
    go x y (a:as) = trace ("x " ++ show x ++ " y " ++ show y ++ " a " ++ show a ++ " solv " ++ (show (min x y + a)) ++ " as " ++ show as)
           go y (min x y + a) as

costs as = let result = zipWith3 (\x y a -> min x y + a) (0:0:result) (0:result) as in result


climbAgain (x:y:zs) = go x y zs 
  where go x y [] = min x y 
        go x y (z:zs) = go y (min x y + z) zs
        