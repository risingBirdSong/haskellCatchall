import Data.List (sortBy, sortOn)
import Data.Ord (compare, comparing)

rotate = drop <> take
-- rotate 3 [1,2,3,4,5]
-- [4,5,1,2,3]
test_lsort =  ["abc","de","fgh","de","ijkl","mn","o"]

-- https://stackoverflow.com/questions/57578855/what-is-the-best-way-to-realize-a-b-as-an-applicative-functor/57580348#57580348
-- https://stackoverflow.com/questions/11486436/composing-two-comparison-functions

composedSort :: [String] -> [String]
composedSort = sortBy $ (comparing length) <> compare

-- in general, whenever the thing you're comparing is expensive to calculate, 
-- sortOn makes sure its only evaluated once
efficientSort xs = sortOn (\x -> (length x, x)) xs

