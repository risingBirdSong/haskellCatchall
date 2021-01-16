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

type Founded = Int
-- number of programmers
type Coders = Int

data SoftwareShop =
  Shop {
  founded :: Founded
  , programmers :: Coders
  } deriving (Eq, Show)


data FoundedError =
  NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)


validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n


validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n


mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
  then Left $ TooManyCodersForYears founded programmers
  else Right $ Shop founded programmers