-- Output: [0,1,9,16,100]
import Data.List
input = [-4,-1,0,3,10]
squars nums = sort $ map (^2) nums 


rmvabc str = map (\x -> if x == 'a' || x == 'b' || x == 'c' then ' ' else x) str