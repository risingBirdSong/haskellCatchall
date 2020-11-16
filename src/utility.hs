import Data.List
-- https://leetcode.com/problems/make-two-arrays-equal-by-reversing-sub-arrays/

takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []

dropUntil _ [] = []
dropUntil trg (l:ls)
  | (trg == l) = ls 
  | otherwise = dropUntil trg ls 

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


distBust nums str dst = sum $ map (snd) $ takeUntil (\x -> (dst == (snd x))) $  zip [str..] nums

distBustasc nums str dst = take (dst - str) $ zip [str..] nums
distBustdes nums str dst = drop (dst - str) $ zip [str..] nums
