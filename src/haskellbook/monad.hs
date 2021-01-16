import Control.Monad
-- andOne x = [x, 1]

-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>) :: m a -> m b -> m b
-- return :: a -> m a


bind :: Monad m => (a -> m b) -> m a -> m b
bind f mon  = join $ fmap f mon 

-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a

say1 = putStrLn $ show 1 
say2 = putStrLn $ show 2 

saytup = (say1, say2)
-- *Main> fst saytup
-- 1
-- *Main> snd saytup
-- 2

-- (>>=) :: [a] -> (a -> [b]) -> [b]

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
  then [x*x, x*x]
  else []

-- *Main> twiceWhenEven [1..3]
-- [4,4]

-- how to inspect a certain specific instance of a type 
--  :set -XTypeApplications
--  :t (>>=) @Maybe