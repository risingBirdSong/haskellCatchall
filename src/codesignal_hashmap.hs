import Data.List
import Data.List.Split
import qualified Data.Map as M
import Debug.Trace
import System.IO.Unsafe
import qualified Data.Set as S
groupingDishes dishes =  map sorting . cleanup $ M.toList . musthavetwoings $ foldr gather (M.empty ) dishes
    where gather (food:ings) themap  = foldr handleingreds  themap ings
            where handleingreds ing map 
                          | M.member ing map = M.adjust (++[food]) ing map  
                          | otherwise = M.insert ing [food] map
          musthavetwoings amap = M.filter ((>=2).length) amap 
          sorting (x:xs) = x : sort xs
          cleanup lst = map (uncurry (:)) lst




dishes = [["Salad", "Tomato", "Cucumber", "Salad", "Sauce"],
        ["Pizza", "Tomato", "Sausage", "Sauce", "Dough"],
        ["Quesadilla", "Chicken", "Cheese", "Sauce"],
        ["Sandwich", "Salad", "Bread", "Tomato", "Cheese"]]

strings = ["cat", "dog", "dog"] 
patterns = ["a", "b", "b"]

stringsa = ["cat", 
 "dog", 
 "doggy"]
patternsa =
  ["a", 
  "b", 
  "b"]
patternsFail = ["a", "b", "bb"]

-- areFollowingPatterns strs pttr = trace ("a" ++ show (group strs)  ++ "b" ++ show (group pttr)) ((length $ group strs) == (length $ group pttr))
areFollowingPatterns xs ys = snd $ foldr (\(x,y) (st,bl) -> (logic x y st bl)) (S.empty, True) (zip xs ys) 
  where logic x y st bl 
               | bl == False = (st, False)
               | (S.member x st) && (S.member y st) = (st, True)
               | (notinSet x st) && (notinSet y st) = (S.insert y (S.insert x st) ,True) 
               | otherwise  =  (st,False)
        notinSet val st = not $ S.member val $ st

c = ["a","b","c"]
cc = ["a","bb","c"]
