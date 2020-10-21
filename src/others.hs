import Data.List (sortBy)
import Data.Ord (compare, comparing)
test_lsort =  ["abc","de","fgh","de","ijkl","mn","o"]

-- https://stackoverflow.com/questions/57578855/what-is-the-best-way-to-realize-a-b-as-an-applicative-functor/57580348#57580348
-- https://stackoverflow.com/questions/11486436/composing-two-comparison-functions

composedSort :: [String] -> [String]
composedSort = sortBy $ (comparing length) <> compare

