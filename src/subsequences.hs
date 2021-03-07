import Data.List
import Debug.Trace

continuousSubSeqs = filter (not . null) . concatMap inits . tails

subsA [] = []
subsA (x:xs) = [x] : foldr f [] (subsA xs)
  where f ys acc = (ys : (x:ys) : acc)

subsB [] = []
subsB (x:xs) = [x] : foldr f [] (subsB xs)
  where f y acc = y : (x:y) : acc

subs []      =  []
subs (x:xs)  =  [x] : foldr f [] (subs xs)
  where f ys acc = trace tracer ( ys : (x : ys) : acc)
          where tracer = (" x> " ++ show x ++ "<x" ++ " ys> " ++ show ys ++ " <ys " ++ "xs>" ++ show xs ++ "")

-- hmm, why cant I show this -> ys : (...) ... cannot construct the infinite type
-- "ys:(x:ys)> " ++ (show ( ys : (x : ys)))

addoneallR x xs = zipWith ((++) . (map (x:))) ([]:xs) (xs ++ [[]])
addoneallL xs x = zipWith ((++) . (map (x:))) ([]:xs) (xs ++ [[]])
subsR xs = concat $ foldr addoneallR [[[]]] xs
subsL xs = concat $ foldl (addoneallL) [[[]]] (reverse xs)

subs'' :: Int -> [a] -> [[a]]
subs'' _ []     = [[]]
subs'' 0 _      = [[]]
subs'' n xs | n >= length xs = [xs]
subs'' n (x:xs) = concat [map (x:) $ subs'' (n-1) xs, subs'' n xs]

str = ["a","b","c"]

shower = mapM_ putStrLn str  

sbs [] = []
sbs (x:xs) = [x] : foldr f [] (sbs xs)
  where f y acc = shwer  
            where shwer =  (y : (x:y) : acc)
-- trace ( show (y : (x:y) : acc) )    

-- [2,3,6,8] length 4
-- he wants this [2,3,4,5,6,7,8] length 7

dpLCS :: String -> String -> Int
dpLCS _ [] = 0
dpLCS a b =
  let nextRow ac prevRow =
        let diagonals = 0:prevRow
            lefts = 0:thisRow
            ups = prevRow
            maxes = zipWith max lefts ups
            thisRow = zipWith3 (\diag maxLeftUp bc ->
                                   if bc == ac then 1 + diag else maxLeftUp)
                                   diagonals maxes b
        in thisRow

      firstRow = map (\_ -> 0) b
      dpTable = firstRow:zipWith nextRow a dpTable

  in last (last dpTable)


dpLCS' _ [] = 0
dpLCS' a b = 
  let nextRow ac prevRow =
        let diagnls = 0:prevRow
            lefts = 0:thisRow 
            ups = prevRow 
            maxes = zipWith max lefts ups 
            thisRow = zipWith3 (\diag maxLeftUp bc ->
                                    if bc == ac then 1 + diag else maxLeftUp)
                                    diagnls maxes b 
        in thisRow 
      firstRow = map (\_ -> 0) b
      dpTable = firstRow: zipWith nextRow a dpTable
 in last (last dpTable)












-- makeArrayConsecutive2 n = length [(minimum n) .. (maximum n)] - length n
--makeArrayConsecutive2 s = (maximum (s) - minimum s + 1) - length s 