import qualified Data.Set as S
import Data.List

-- https://leetcode.com/problems/unique-number-of-occurrences/
-- length seti == length grouped
unq xs = length seti == length grouped
    where seti = S.fromList xs
          grouped = S.fromList $ map length $ group $ sort xs