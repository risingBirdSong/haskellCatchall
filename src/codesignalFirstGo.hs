-- https://app.codesignal.com/interview-practice/task/pMvymcahZ8dY4g75q/description
-- {-# LANGUAGE FlexibleContexts #-}

import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Matrix as Mtx
import qualified Data.Map as M 


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






firstNotRepeatingCharacter strs = finder strs options
   where options = concat $ filter ((==1).length) . group $ sort strs 
         finder [] cands = '_'
         finder (x:xs) cands 
              | x `elem` cands = x 
              | otherwise = finder xs cands 

firstNotRepeatingCharacter' :: String -> Char
firstNotRepeatingCharacter' [] = '_'
firstNotRepeatingCharacter' (x:xs) | x `elem` xs = firstNotRepeatingCharacter (filter (/=x) xs)
                                  | otherwise = x 

firstNotRepeatingCharacter'' s = go s s
    where go _ [] = '_'
          go s (x:xs)
                  | length (filter (==x) s) == 1 = x
                  | otherwise = go newLs newXs        
              where newLs = filter (/= x) s
                    newXs = filter (/= x) xs


firstNotRepeatingCharacter''' = go S.empty
    where go _ [] = '_'
          go repeats (x:xs) = 
            if (S.member x repeats) || (elem x xs)
                then go (S.insert x repeats) xs
                else x


options strs = concat $ filter ((==1).length) . group $ sort strs 

-- "abbbbcddffghhhhhiiijjkkkklnnnoopppqqrrsssttuvvxxxxyy"
-- "ngrhhqbhnsipkcoqjyviikvxbxyphsnjpdxkhtadltsuxbfbrkof"

-- an idea from melissa regaring the baf
-- fnrc xs = let bag = Map.fromListWith (+) . zip xs $ repeat 1 in find ((== 1) . (bag !)) xs



counting val cur = if val == cur then (Just (+1)) else Nothing 

  -- where myMap = M.fromList []
        -- go [] count = count 
        -- go (x:xs) count = M.updateWithKey (counting x) count 

countElems xs = M.fromListWith (+) $ zip xs (repeat 1)

countA xs = M.fromListWith (+) $ map (\x -> (x,1)) xs
countB zs = map (\xs -> (head xs, length xs)) . group . sort $ zs 