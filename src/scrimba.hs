import Data.List
import Data.List.Split
import Data.Ord
strs = ["abc", "", "aaa", "a", "zz"];

-- sortByLength ls = sortBy (comparing length) ls
values = ["a", "b", "c", "d","e", "f"];
sortByLength ls = sortOn length ls 


chunkyMonkey chnk [] = []
chunkyMonkey chnk lst = (take chnk lst) : chunkyMonkey chnk (drop chnk lst)