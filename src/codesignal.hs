-- https://app.codesignal.com/interview-practice/task/pMvymcahZ8dY4g75q/description
import Data.List

firstDuplicateNestedlambdas xs = groupBy (\(x, v1) -> \(y,v2) -> x == y ) . sort $ zip xs [0..]  
firstDuplicateCollapsed xs = groupBy (\(x, v1) (y,v2) -> x == y ) . sort $ zip xs [0..]  

firstDuplicate xs = groupBy (\(x, v1) (y,v2) -> x == y ) . sort $ zip xs [0..]  
