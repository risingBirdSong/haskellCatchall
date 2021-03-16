{-# LANGUAGE DeriveFoldable #-}

import Data.Maybe
import Data.List


data Tree a = Null | Tree a (Tree a) (Tree a) deriving (Show, Eq, Ord, Foldable)

instance Functor Tree where
    fmap f Null = Null
    fmap f (Tree a x y) =  Tree (f a) (fmap f x) (fmap f y)

-- fmap f bt





example = Tree 1 (Tree 2  (Tree 4 Null Null) Null) (Tree 3 (Tree 5 Null Null) (Tree 6 Null Null))
restoreexlist = Tree [1] (Tree [2]  (Tree [4] Null Null) Null) (Tree [3] (Tree [5] Null Null) (Tree [6] Null Null))

children :: Tree a -> [Tree a]
children Null = []
children (Tree _ l r) = [l, r]

value :: Tree a -> Maybe a
value Null = Nothing
value (Tree v _ _) = Just v

-- rows :: Tree a -> [[a]]
-- rows = takeWhile (not . null) . fmap (mapMaybe value) . iterate (concatMap children)
-- rows' :: Foldable t => t (Tree Integer ) -> [Tree Integer]
-- rows' xs= (concatMap children xs)

-- solution :: Ord a => Tree a -> [a]
-- solution xs = map maximum . rows xs

mergeMax :: Ord a => [a] -> [a] -> [a]
mergeMax (x:xs) (y:ys) = max x y : mergeMax xs ys
mergeMax xs [] = xs
mergeMax [] ys = ys

solution' :: Ord a => Tree a -> [a]
solution' Null = []
solution' (Tree v l r) = v : mergeMax (solution' l) (solution' r)


list1 = [1]


listRow Null = repeat []
listRow (Tree v l r) = [v] : zipWith (<>) (listRow l) (listRow r)
solver tree = map maximum . takeWhile (not . null) $ listRow tree 



listNum Null = repeat []
listNum (Tree v l r) = [v] : zipWith (<>) (listNum l) (listNum r)

takelistnum t = takeWhile (not . null) $ listNum t 


        -- 1
      -- 2   3
  --  4     5 6


listNum' (Tree v Null Null) = [[v]]
listNum' (Tree v Null r) = map (v :) (listNum' r)
listNum' (Tree v l Null) = map (v :) (listNum' l)
listNum' (Tree v l r) =  map (v:) $ (listNum' l) ++ (listNum' r)

-- digitTreeSum' :: Tree Int -> [[Int]]
digitTreeSum' (Tree v Null Null) = [[v]]
digitTreeSum' (Tree v Null r)    = map (v :) (digitTreeSum' r)
digitTreeSum' (Tree v l    Null) = map (v :) (digitTreeSum' l)
digitTreeSum' (Tree v l    r)    = [l, r] >>=  (\x -> map (v :) (digitTreeSum' x))


-- digitTreeSum' (Tree v l    r)    = [l, r] >>= (\x -> map (v :) (digitTreeSum' x))