import Data.Ord
import Data.List
-- (\x y -> (x, reverse y))
output tup = (\(x , y) -> (x, reverse y)) tup
caller nums = output $ handler nums [] [] 
handler [] acc options = (acc,options)
handler [x] acc options = (acc,options)
handler (a:b:ns) acc options = handler ((head $ mostOfChained a b):ns) ((head $ mostOfChained a b):ns) (mostOfChained a b :options)
  

maxWithTie ls = head $ group $ sortBy (comparing Down)  ls

-- (\f-> f x y )
chainedFuncs x y = map (\f -> f x y) [(*),(+),(-), take2nd]
take2nd _ y = y

mostOfChained x y = maxWithTie $ chainedFuncs x y

-- [Just 0,Just 9,Just 4,Just 0]

upToN n = [1..n]