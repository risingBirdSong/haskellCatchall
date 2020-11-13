import Data.List
takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []

dropUntil trg (l:ls)
  | (trg == l) = ls 
  | otherwise = dropUntil trg ls 
-- Input: target = [1,2,3,4], arr = [2,4,1,3]

start = [2,4,1,3]
a = [1,4,2,3]
-- sub :: (Foldable t, Eq a) => a -> t a -> [a]
sub trg ls drp = reverse $ drop drp $ takeUntil (== trg) ls
worker trg ls drp = take drp ls ++ (sub trg ls drp) ++ dropUntil trg ls 

grouper t ls drp =  reverse (sub t ls drp)

-- ans [] arr = ori == arr
-- ans (g:goal) arr = ans goal (:) 
--   where ori = (g:goal)