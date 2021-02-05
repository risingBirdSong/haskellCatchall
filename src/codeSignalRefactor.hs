import Data.List
import qualified Data.Set as S


firstDuplicate as = go as S.empty   
  where go [] set = -1
        go (x:xs) set 
          | S.member x set = x
          | otherwise = go xs (S.insert x set)