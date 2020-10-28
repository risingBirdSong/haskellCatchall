import Debug.Trace
import Data.List as L
class C a where
  m                     :: Show b => a -> b

-- class C a where m :: Show b => a -> b you can read as
-- "If you want to write an instance of the type class C for any type a, you have to provide an implementation of m :: Show b => a -> b"
-- You can do that by writing, for example

-- Here's an example type constructor
data Pair a = MkPair a a
-- Oh let me be less ambiguous
-- Again, Pair is a type constructor
-- MkPair is not
-- It's a constructor
-- Not a type constructor
-- Big difference!!!


-- i did get taught what you wrote

split' :: Eq a => a -> [a] -> [[a]]
split' c [] = []
split' c xs = xs' : if null xs'' then [] else split' c (tail xs'')
    where xs' = takeWhile (/=c) xs
          xs''= dropWhile (/=c) xs
-- split' c xs | trace ("showing" ++ show xs) False = undefined


taker c xs = takeWhile (/=c) xs
dropper c xs = dropWhile (/=c) xs

aaa = "codeleet"
bbb = [4,5,6,7,0,2,1,3]

shfl' :: (Ord a, Ord b) => [a] -> [b] -> [b]
shfl' = ((map snd . L.sort) . ). zip