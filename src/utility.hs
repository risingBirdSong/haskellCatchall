import Data.List
takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []

dropUntil _ [] = []
dropUntil trg (l:ls)
  | (trg == l) = ls 
  | otherwise = dropUntil trg ls 
-- Input: target = [1,2,3,4], arr = [2,4,1,3]

start = [2,4,1,5]
a = [1,4,2,3]
b = [1,2,4,3]
-- sub :: (Foldable t, Eq a) => a -> t a -> [a]
sub trg ls drp = reverse $ drop drp $ takeUntil (== trg) ls
worker trg ls drp = take drp ls ++ (sub trg ls drp) ++ dropUntil trg ls 

solver trgArr arr = solver' trgArr trgArr arr 0
solver' [] trgArr arr _ = False
solver' (r:rcs) (trgArr) arr cnt
  | (trgArr) == arr = True
  | otherwise = solver' rcs (trgArr) (worker r arr cnt) (cnt + 1)


-- ans [] arr = ori == arr
-- ans (g:goal) arr = ans goal (:) 
--   where ori = (g:goal)