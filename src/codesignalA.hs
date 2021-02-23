import Data.List
import Data.Char 
import qualified Data.Map as M
import qualified Data.Vector as V 
import Control.Lens 


counter xs = go xs (M.empty)
  where go [] mp = mp 
        go (x:xs) mp
          | M.member x mp = go xs (M.adjust (+1) x mp) 
          | otherwise = go xs (M.insert x 1 mp)

foldCount xs = foldr logic (M.empty) xs 
    where logic x mp 
              | M.member x mp = M.adjust (+1) x mp
              | otherwise = M.insert x 1 mp 