import System.IO
import Data.List

matchHouses :: (Num p, Ord p) => p -> p
matchHouses x 
  | x == 1 = 6
  | x > 1 = 6 + ((x-1) * 5)


  -- Add up the Numbers from a Single Number
  -- https://edabit.com/challenge/4gzDuDkompAqujpRi
addUp x = sum [1..x]

-- Which Function Returns the Larger Number?
-- https://edabit.com/challenge/o7TwicAHWuMkjbDqQ


rtrn5 :: () -> Integer
rtrn5 = (\_ -> 5)
rtrn6 :: () -> Integer
rtrn6 = (\_ -> 6)


largerFun x y = max x y

lf :: (() -> Integer) -> (() -> Integer) -> Char
lf x y 
  | x() > y() = 'f'
  | otherwise = 'g'