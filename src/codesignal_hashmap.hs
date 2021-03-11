import Data.List
import Data.List.Split
import qualified Data.Map as M

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


-- areFollowingPatterns strings patterns = 