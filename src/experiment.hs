import Data.Function
import Data.List 
import Data.Ord
maxDivisorsUntil :: Integer -> Integer
-- maxDivisorsUntil n = fst $ maximumBy (compare `on` snd)  (zip $ [1..n] map (\n -> length (filter ((==0) . (mod n)) [1..n])) [1..n])
maxDivisorsUntil n =  (zip  [1..n] ( map (\n -> length (filter ((==0) . (mod n)) [1..n])) [1..n]))