import Data.List
import Data.Ord
maximus n ls = take n (sortOn Down ls)