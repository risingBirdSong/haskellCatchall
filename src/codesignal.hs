-- https://app.codesignal.com/interview-practice/task/pMvymcahZ8dY4g75q/description
-- {-# LANGUAGE FlexibleContexts #-}

import Data.List
import Data.Ord

firstDuplicateNestedlambdas xs = groupBy (\(x, v1) -> \(y,v2) -> x == y ) . sort $ zip xs [0..]  
firstDuplicateCollapsed xs = groupBy (\(x, v1) (y,v2) -> x == y ) . sort $ zip xs [0..]  

firstDuplicate xs = solve go 
  where go =  sortBy (\x y -> compare (snd $ last x) (snd $ last y)) . filter ((>1).length) . groupBy (\(x, v1) (y,v2) -> x == y ) . sort $ zip xs [0..]  
        solve [] = -1
        solve ([(x,i), _] : xs) = x
