import Data.List

muls3and5 = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [0..999]