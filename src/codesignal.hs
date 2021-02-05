-- https://app.codesignal.com/interview-practice/task/pMvymcahZ8dY4g75q/description
import Data.List

firstDuplicate xs = groupBy (\(x, v1) -> \(y,v2) -> x == y ) . sort $ zip xs [0..]  

