import Data.List
grida = [[4,3,2,-1],[3,2,1,-1],[1,1,-1,-2],[-1,-1,-2,-3]]

testa = [7,6,5,4,3,2,1,-1]

filternegs lst = filter (\x -> x < 0) lst
solve mtx = length (concat (map (filternegs) mtx))

practice =  filter (>5) [1,2,3,4,5,6,7,8]


-- Input: [[1,1,0],[1,0,1],[0,0,0]]
-- Output: [[1,0,0],[0,1,0],[1,1,1]]

rrvssub mtrx = map (map invert) (map (reverse) mtrx)

invert num 
  | (num == 0) = 1
  | otherwise = 0

oneszeros = [[1,1,0],[1,0,1],[0,0,0]]


both lst = map (invert) lst

-- 1450. Number of Students Doing Homework at a Given Time

startTime = [1,2,3,3]
endTime = [3,2,7,8] 
queryTime = 4

qt (str) (end) (q) = length (concat (qt' (str) (end) (q) (0)))
qt' [] _ _ _  = []
qt' _ [] _ _ = []
qt' (s:xs) (e:ys) q acc 
  | ((e > q) && (s < q)) = [acc+1] : qt' (xs) (ys) q (acc+1)
  | otherwise = qt' (xs) (ys) q  (acc)


-- 1588. Sum of All Odd Length Subarrays
arra = [1,4,2,5,3]
-- [[1], [1,4], [1,4,2], [1,4,2,5], [1,4,2,5,3], [4], [4,2]]

oddsubs arr = sum $ map (sum)  $ filter (odd . length) $ filter (flip isInfixOf arr) $ subsequences arr