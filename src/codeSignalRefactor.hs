import Data.List
import qualified Data.Set as S
import Data.Foldable
import Data.Function (on)


firstDuplicate as = go as S.empty   
  where go [] set = -1
        go (x:xs) set 
          | S.member x set = x
          | otherwise = go xs (S.insert x set)

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn f = maximumBy (compare `on` f)

minimumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
minimumOn f = minimumBy (compare `on` f)


-- firstNotRepeatingCharacter "" = '_'
firstNotRepeatingCharacter str = go
   where go = solve $ findmins prepare 
         prepare = concat . filter ((==1).length)
          . groupBy (\(x,_)(y,_) -> x == y) 
          $ sort $ zip str [0..]
         findmins [] = Nothing  
         findmins vals = Just ( fst $ minimumOn snd vals) 
         solve Nothing = '_'
         solve (Just x) = x

fnrcLogic str = concat . filter ((==1).length)
          . groupBy (\(x,_)(y,_) -> x == y) 
          $ sort $ zip str [0..]