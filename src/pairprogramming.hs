-- reverse 

-- https://www.codewars.com/kata/523a86aa4230ebb5420001e1/train/haskell

-- factorial


myLast_ :: [p] -> p
myLast_ [x] = x
myLast_ (x:xs) = myLast_ xs

--  [1 2 3]
--   [2 3]
--     [3]


-- numSplit :: Fractional a => a -> [a]
-- numSplit x `mod` 2 == 0 = [round x, round x]   
-- numSplit x 
--   | (x `mod` 2 == 0) = [round x, round]
--   | (x `mod` 2 /= 0) = [floor x, ceiling x]

-- invincible
-- numSplit :: (RealFrac a1, Integral a1, Integral a2) => a1 -> [a2]
-- numSplit :: (Num a1) => a1 -> [a2]
-- numSplit :: (RealFrac a1, Integral a1, Integral a2) => a1 -> [a2]
-- numSplit :: (RealFrac a, Integral b) => a -> [b]
-- numSplit :: (RealFrac a1, Integral a1, Integral a2) => a1 -> [a2]
-- numSplit :: (Integral a) => a -> [a]
numSplit :: Integral a => a -> [a]
numSplit x 
  | even x = [(x `div` 2), (x `div` 2)]
  | odd x = [x `div` 2, (x `div` 2) + 1]

rvrs :: [a] -> [a]
rvrs [] = [] -- end case , base case, exit condition 
rvrs (start:rest) = rvrs (rest) ++ [start] -- doing magic, doing the work

ttt = floor 5.5 