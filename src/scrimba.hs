import Data.List
import Data.Ord
strs = ["abc", "", "aaa", "a", "zz"];

-- sortByLength ls = sortBy (comparing length) ls
sortByLength ls = sortOn length ls 