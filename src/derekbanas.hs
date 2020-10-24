import System.IO
import Data.List
boundedInt = maxBound :: Int -- 9223372036854775807

-- Integer is unbounded whole number can be as big as memory can hold, huge number

-- prefer Doubles over floats, up to 11 points decimal precision


-- prefix vs infix
moda = mod 5 4
modb = 5 `mod` 4


negNumsNeedParens = 5 + (-4)

-- sqrt :: Floating a => a -> a
myNumA = 16 :: Int
-- in order to work with sqrt which takes Floating, we must convert
aaa = sqrt (fromIntegral myNumA )-- 4.0

myNums = [3,5,7,9,11]
-- Check if value is in list
is7InList = 7 `elem` myNums -- True

-- Get max value
maxPrime = maximum myNums -- 11
 
-- Get minimum value
minPrime = minimum myNums -- 3

-- You can use letters as well
letterList = ['A','C'..'Z'] -- "ACEGIKMOQSUWY"

infA = [11,22..]
access50 = infA !! 50 -- 561

-- We can filter the results with conditions
listTimes3 = [x * 3 | x <- [1..20], x*3 <= 50]
 
-- Return all values that are divisible by 13 and 9
divisBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]