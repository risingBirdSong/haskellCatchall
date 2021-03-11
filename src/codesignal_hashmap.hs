import Data.List
import Data.List.Split
import qualified Data.Map as M
import Debug.Trace
import System.IO.Unsafe
import qualified Data.Set as S
import Data.Foldable
import Data.Maybe
import Data.Function

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


-- (cat : a) (dog : b) (dog : bb)


stringsa = ["cat", 
 "dog", 
 "doggy"]
patternsa =
  ["a", 
  "b", 
  "b"]

setNub xs = S.toList $ S.fromList xs
areFollowingPatterns xs ys = (length $ setNub xs) == (length $ setNub ys) 

recurFun [] _ = True
recurFun ((x, y) : xs) mp = 
    case M.lookup x mp of
         Just val -> val == y && recurFun xs mp 
         Nothing -> recurFun xs (M.insert x y mp)

areFollowingPatterns' strings patterns = 
    (recurFun (zip strings patterns) M.empty)
    && (recurFun (zip patterns strings) M.empty)
    
getAll :: [String] -> [[Int]]
getAll x = sort . map (map snd) . groupBy (\a b -> fst a == fst b) . sort . zip x $ [0..]

areFollowingPatterns'' strings patterns = getAll strings == getAll patterns

areFollowingPatterns''' strings patterns = halfSolve strings patterns && halfSolve patterns strings
halfSolve a b = checkInsert M.empty $ zip a b
checkInsert acc list = case list of
 []        -> True
 (p, s):xs -> case M.lookup p acc of
   Nothing -> checkInsert (M.insert p s acc) xs
   Just a  -> case (a==s) of
    False  -> False
    True   -> checkInsert acc xs
 

areFollowingPatterns'''' strings patterns =
    all (/= Nothing) (M.elems s) && all (/= Nothing) (M.elems p)
  where s = M.fromListWith invalidate $ zip strings $ map Just patterns
        p = M.fromListWith invalidate $ zip patterns $ map Just strings
        invalidate x y | x == y    = x
                       | otherwise = Nothing

safeInsert k a m = case M.lookup k m of
    Nothing -> return $ M.insert k a m
    Just v  -> if v == a then return m else Nothing

safeFromList vs m = foldrM (uncurry safeInsert) m vs

areFollowingPatterns''''' s p =
    let check a b = 
            isJust $ safeFromList (zip a b) M.empty
    in check s p && check p s

indexate::Ord a => [a] -> [Int]
indexate = traceShowId . snd . mapAccumL mark (M.empty, 1) where
    mark (m, i) e = maybe ((M.insert e i m, i + 1), 0) ((,) (m, i)) $ M.lookup e m

areFollowingPatterns'''''' strings patterns = indexate strings == indexate patterns

areFollowingPatterns''''''' strings patterns = ok strings patterns && ok patterns strings
    where
    ok l1 = all same. groupBy ((==) `on` fst). sort. zip l1
        where same = and. (\l -> zipWith (==) l (tail l))

stA = ["re", 
        "jjinh", 
        "rnz", 
        "frok", 
        "frok", 
        "hxytef", 
        "hxytef", 
        "frok"]
psA = ["kzfzmjwe", 
          "fgbugiomo", 
          "ocuijka", 
          "gafdrts", 
          "gafdrts", 
          "ebdva", 
          "ebdva", 
          "gafdrts"]


stB = ["kwtfpzm", 
 "kwtfpzm", 
 "kwtfpzm", 
 "kwtfpzm", 
 "kwtfpzm", 
 "wfktjrdhu", 
 "anx", 
 "kwtfpzm"]

ptB =["z", 
    "z", 
    "z", 
    "hhwdphhnc", 
    "zejhegjlha", 
    "xgxpvhprdd", 
    "e", 
    "u"]