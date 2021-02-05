-- https://app.codesignal.com/interview-practice/task/pMvymcahZ8dY4g75q/description
-- {-# LANGUAGE FlexibleContexts #-}

import Data.List
import Data.Maybe
-- import Data.Ord

firstDuplicateNestedlambdas xs = groupBy (\(x, v1) -> \(y,v2) -> x == y ) . sort $ zip xs [0..]  
firstDuplicateCollapsed xs = groupBy (\(x, v1) (y,v2) -> x == y ) . sort $ zip xs [0..]  

firstDuplicate xs = solve go 
  where go =  sortBy (\x y -> compare (snd $ grabSecond x) (snd $ grabSecond y)) . filter ((>1).length) . groupBy (\(x, v1) (y,v2) -> x == y ) . sort $ zip xs [0..]  
        solve [] = -1
        -- solve ([(x,i), _] : _) = x
        solve (((x,i) : _ ) : _ ) = x
        grabSecond (x:y:zs) = y


firstDInnerLogic xs = sortBy (\x y -> compare (snd $ grabSecond x) (snd $ grabSecond y)) . filter ((>1).length) . groupBy (\(x, v1) (y,v2) -> x == y ) . sort $ zip xs [0..]

grabSecond (x:y:zs) = y



tupleMatching [(x,y)] = "oneTuple"
tupleMatching [(x,y),(xx,yy)] = "twoTuple"
-- tupleMatching [(x,y),(xx,yy), _] = "moreThanTwoTuples"
tupleMatching _ = "moreThanTwoTuples"

ex = [2, 1, 3, 5, 3, 2]
solverPattern (x:xs) = x 

-- getFirstFromTuples (([(x,i) : _]) : _) = x
getTupleInn (((x,i) : _ ) : _ ) = x

-- *Main> solverPattern [[(1,2),(3,4)],[(4,5),(6,7)]]
-- [(1,2),(3,4)]






firstNotRepeatingCharacter strs = head $ fromMaybe "_"  first
   where options = filter ((==1).length) . group $ sort strs 
         first = find (`elem` options) options

-- "abbbbcddffghhhhhiiijjkkkklnnnoopppqqrrsssttuvvxxxxyy"
-- "ngrhhqbhnsipkcoqjyviikvxbxyphsnjpdxkhtadltsuxbfbrkof"