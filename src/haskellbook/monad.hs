import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
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

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)
instance Functor (Sum a) where
  fmap f (Second b) = Second (f b) 

instance Applicative (Sum a) where
  pure val = Second val 
  (<*>) (First a) _ = (First a)
  (<*>)  _ (First b) = (First b)
  (<*>) (Second f) (Second b) = (Second (f b)) 

instance Monad (Sum a) where
  return = pure  
  (>>=) mn f = join (fmap f mn) 


data Nope a =
  NopeDotJpg deriving (Show, Eq)

instance Functor Nope where 
  fmap _ NopeDotJpg = NopeDotJpg


instance Applicative Nope where 
  pure x = NopeDotJpg
  (<*>) (NopeDotJpg) (NopeDotJpg) = NopeDotJpg

instance Monad Nope where 
  return x = NopeDotJpg
  (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq


testWithTrigger trigger = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


monadSpec :: IO ()
monadSpec = do
  testWithTrigger (undefined :: Nope (Int, Int, Int))

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a) 
instance Applicative Identity where
  pure v = Identity v 
  (<*>) (Identity f) (Identity v) = Identity (f v) 
instance Monad Identity where
  return = pure
  -- (>>=) m f = join (fmap f m)
  (>>=) (Identity a) f = f a


instance Arbitrary a => Arbitrary (Identity a) where 
  arbitrary = do 
  a <- arbitrary
  return (Identity a)

instance (Eq a) => EqProp (Identity a) where 
  (=-=) = eq

myidtest = do
  putStrLn "type" 
  a <- getLine
  let test = (Identity a) 
  return (test)

data List a =
  Nil
  | Cons a (List a) deriving (Show, Eq)

instance Semigroup (List a) where 
  (<>) Nil xs = xs 
  (<>) xs Nil = xs 
  (<>) (Cons x xs) (ys) = Cons x (xs <> ys)

instance Monoid (List a) where 
  mempty = Nil

-- *Main> quickBatch $ monoid (undefined :: List (String))

-- monoid:
--   left  identity: +++ OK, passed 500 tests.
--   right identity: +++ OK, passed 500 tests.
--   associativity:  +++ OK, passed 500 tests.
--   mappend = (<>): +++ OK, passed 500 tests.
--   mconcat:        +++ OK, passed 500 tests.

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons aa ls) = (Cons (f aa) (fmap f ls))

-- instance Monoid

instance Applicative List where 
  pure x = (Cons x Nil)
  (<*>) _ Nil = Nil  
  (<*>) Nil _ = Nil 
  (<*>) (Cons f fs) (Cons x xs) =  Cons (f x) ((<*>) fs xs)
   

instance Monad (List) where 
  return = pure 
  (>>=) (Nil) _ = Nil 
  (>>=) (Cons x Nil) f = f x
  (>>=) (Cons x xs) f = (f x) <> ((>>=) xs f)

genList :: (Arbitrary a) => Gen (List a) 
genList = do 
  a <- arbitrary
  l <- genList
  frequency [(1, return Nil), (5, return (Cons a l))]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

instance (Eq a) => EqProp (List a) where
  (=-=) = eq 