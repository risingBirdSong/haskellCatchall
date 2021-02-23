import Data.List
import Data.Char 
import qualified Data.Map as M
import qualified Data.Vector as V 
import Control.Lens 

foldCount xs = foldr logic (M.empty) xs 
    where logic x mp 
              | M.member x mp = M.adjust (+1) x mp
              | otherwise = M.insert x 1 mp 

files = ["doc", "doc", "image", "doc(1)", "doc"]
countFiles files = go files (M.empty)
  where go [] mp = []
        go (f:fs) mp 
              | M.member f mp = case (M.lookup f mp) of 
                                      Just x 
                                         | x == 1 -> (f ++ "(" ++ (show x) ++ ")") : go fs (M.adjust (+1) f mp)
                                         | x > 1 -> (f ++ "(" ++ (show x) ++ ")(" ++ (show x) ++ ")") : go fs (M.adjust (+1) f mp)
                                      Nothing -> go fs (M.insert "error" 1 mp)
              | otherwise = f : go fs (M.insert f 1 mp)

aa =  [[4, 1],
                [4, -1],
                [0, 0],
                [6, 1]]

bb = [[3,1], 
      [6,1], 
      [4,1], 
      [5,1]]

companyBotStrategy trn =  case (map head $ filter (\[v,crct] -> crct == 1) trn) of 
                                [] -> 0.0
                                xs -> (realToFrac $ sum xs) / (realToFrac $ length xs)