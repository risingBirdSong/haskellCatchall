import qualified Data.IntSet as S
import Control.Applicative
 
solve1 n xs = foldr f (const Nothing) xs S.empty where
    f x k s = if S.member (n-x) s then Just (x * (n-x)) else k (S.insert x s)
 
para f z [] = z
para f z (x:xs) = f x xs (para f z xs)
 
solve2 n = para (\x xs r -> fmap (x*) (solve1 (n-x) xs) <|> r) Nothing
 
main = do
    inp <- fmap read . lines <$> readFile "adventofcode//day1.txt"
    print (solve1 2020 inp)
    print (solve2 2020 inp)

-- more newb friendly from Assaf
solve1 n xs = go S.empty xs where
    go _ [] = Nothing
    go s (x:xs) = if S.member (n-x) s then Just (x * (n-x)) else go (S.insert x s) xs