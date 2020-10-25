import System.IO
import Data.List
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

-- import Text.Regex.TDFA

-- in package.yamsl
-- - regex-tdfa-text
-- error ->
-- regex-tdfa-text needed, but the stack configuration has no specified version  (latest matching version       
--                     is 1.0.0.3)

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


binary x =  putStrLn $ showIntAtBase 2 intToDigit x "" 