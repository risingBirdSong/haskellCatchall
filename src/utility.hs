import Data.List
-- https://leetcode.com/problems/make-two-arrays-equal-by-reversing-sub-arrays/
import Control.Monad (join)
import Control.Arrow ((***))
import Control.Arrow 
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

distBustasc nums str dst = sum $ map snd $ take (dst - str) $ zip [str..] nums
distBustdes nums str dst = sum $ map snd $ drop (dst - str) $ zip [str..] nums

distBusanswer nums str dst = min (distBustasc nums str dst) (distBustdes nums str dst)

distBusanswera nums str dst =  uncurry (min) $ mapTuple_ (sum) $ splitAt (dst) nums

-- found great answer to what I was looking for, applying a function two a and b of a tuple
-- just what I need for distBusanswera 
-- https://stackoverflow.com/questions/9722689/haskell-how-to-map-a-tuple
mapTuple :: Arrow a => a b' c' -> a (b', b') (c', c')
mapTuple tupl = join (***) tupl

--  mapTuple (sum) ([1,2],[3,4]) -> (3,7)

-- a "hand made" mapTuple, good to see multiple implementations 
mapTuple_ :: (a -> b) -> (a, a) -> (b, b)
mapTuple_ f (a1, a2) = (f a1, f a2)
-- mapTuple_ (sum) ([1,2],[3,4]) -> (3,7)

firstTest tup =  (first) tup
-- firstTest sum  ([3,4],[1,2]) -> (7,[1,2])

secondTest tup = second tup
--  secondTest  sum  ([3,4],[1,2]) -> ([3,4],3)


