import Data.List
import Data.List.Split

import Data.Maybe

import qualified Data.Set as St
-- https://leetcode.com/problems/make-two-arrays-equal-by-reversing-sub-arrays/
import Control.Monad (join)
import Control.Arrow ((***))
import Control.Arrow 
import Data.Function
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

distBusanswera nums str dst =  uncurry (min) $ mapTuple_ (sum) $ splitAt (dst - str) nums

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


forward  stt end ls = take (end - stt) (drop stt ls)  
backward stt end ls= (take stt ls) ++ drop  (end) ls

-- forward [1,2,3,4,5] 2 4 -> [3,4]
-- backward  [1,2,3,4,5] 2 4  -> [1,2,5]

slicer from to ls = take (to - from +1) $ drop from ls

-- 389. Find the Difference

dif super sub = super \\ sub

recycle bottles exc acc 
  | (bottles <= exc) = acc
  | (bottles >= 0) =  recycle (uncurry (+) (bottles `divMod` exc)) exc (acc + bottles `div` exc)

-- recyclea bottles exc = foldl

-- 1624. Largest Substring Between Two Equal Characters

findDupe ls = ls \\ (St.toList (St.fromList ls))

-- lrgstSub [] cnt  = cnt 
lrgstSub word = lrgstSub' word (findDupe word)
lrgstSub' word "" = -1
lrgstSub' word dupe = foldl (\acc x -> if ([x] /= dupe) then (acc + 1) else acc) 0 word


myFold f acc [] = acc 
myFold f acc (l:ls) = myFold f (f l acc) ls 

-- myFold (+) 0 [1,2,3,4]
-- 10

lrgSub_ ls = lrgSub_' ls (findDupe ls)
-- lrgSub_' [] _ = -1
-- lrgSub_' _ [] = -1
lrgSub_' ls dupe 
  | ls == [] || dupe == [] = -1 
  | otherwise = length $ head $ filter (/= "") $ splitOn dupe ls 

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibs_ = scanl (+) 0 (1:fibs_)

scantest nums = scanl (+) 0 nums



-- 1331. Rank Transform of an Array
rankd = [40,10,20,30]
rankt ls = sort $ map (\(i,v) -> (i,i+1)) $ sortBy (compare `on` snd) $  zip [0..] ls
rankda = [(0,40),(1,10),(2,20),(3,30)]
rankdb = [(1,10),(2,10),(3,30),(0,40)]
rankgoal = [(1,1),(2,2),(3,3),(0,4)]
testMapAccum tupls = mapAccumL (\cnt (i,v) -> (cnt + 1, (i, cnt))) 1 tupls
-- (5,[(1,1),(2,2),(3,3),(0,4)])
rankThem tupls = snd $ mapAccumL (\cnt (i,v) -> (cnt + 1, (i, cnt))) 1 tupls
-- [(1,1),(2,2),(3,3),(0,4)]
sortThemBack tupls = sortBy (compare `on` fst) tupls
-- [(0,4),(1,1),(2,2),(3,3)]
rankLastStep tpls = map (snd) tpls

-- redo the function now after learning
ranked nums = map (snd) $ sortBy (compare `on` fst) $ snd $ mapAccumL (\cnt (i,v) -> (cnt+1, (i,cnt))) 1 $ sortBy (compare `on` snd) $ zip [0..] nums
-- [4,1,2,3]

ranked_a nums = map (snd) $ sortBy (compare `on` fst) $ zip (map (fst) $ sortBy (compare `on` snd) $ zip [0..] nums) [1..]

-- tayacms = zip (map fst step_a) [1..]
-- I try to never increment counters :smile:

-- here's the problem 
-- scantesta = scanl (\acc (x,y) -> (acc + 1) (_, acc + 1)) 0 rankdb

-- here's a wonderful func suggestion i didn't know about 
-- http://zvon.org/other/haskell/Outputlist/mapAccumL_f.html

-- simple example 
-- mapAccumL (\x y -> (x+1,y+x)) 1 [9,6,3]
-- (4,[10,8,6])

-- Input: arr = [40,10,20,30]



-- Output: [4,1,2,3]

