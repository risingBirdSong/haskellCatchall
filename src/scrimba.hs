import Data.List
import Data.List.Split
import Data.Ord
strs = ["abc", "", "aaa", "a", "zz"];

-- sortByLength ls = sortBy (comparing length) ls
values = ["a", "b", "c", "d","e", "f"];
sortByLength ls = sortOn length ls 


chunkyMonkey chnk [] = []
chunkyMonkey chnk lst = (take chnk lst) : chunkyMonkey chnk (drop chnk lst)


-- javascript solution
-- function chunkyMonkey (values, size) {
-- return [values.slice(0, size)].concat(values.slice(size).length === 0 ? [] : chunkyMonkey(values.slice(size), size))
-- }

-- console.log(chunkyMonkey([1,2,3,4,5,6], 4))