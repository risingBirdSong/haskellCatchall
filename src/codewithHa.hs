import Data.List


-- testarr = [6, -2, -5, 7, 3 ]


adjacentElementsProduct xs = maximum $ zipWith (*) (xs)(tail xs)

-- Ratiorg got statues of different sizes as a present from CodeMaster for his birthday, each statue having an non-negative integer size. Since he likes to make things perfect, he wants to arrange them from smallest to largest so that each statue will be bigger than the previous one exactly by 1. He may need some additional statues to be able to accomplish that. Help him figure out the minimum number of additional statues needed.
-- Example
-- For statues = [6, 2, 3, 8], the output should be
-- makeArrayConsecutive2(statues) = 3.
-- Ratiorg needs statues of sizes 4, 5 and 7.
statuesexample =  [6, 2, 3, 8]
--            sort    [2,3,6,8]
--             gaps   [2,3,__ ,  __ 6,__8]
--                           

-- Make Array Consecutive 2
-- makeArrayConsecutive2 
make statues = goal - (length statues)
  where goal = length $ [(minimum statues)..(maximum statues)]