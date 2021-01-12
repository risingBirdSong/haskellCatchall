import Control.Applicative
import Data.List
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- oooh interesting
testing = do 
  let result = (++) <$> getLine <*> getLine
  (fmap length result)
-- the fmap has reached inside IO monad

added :: Maybe Integer
added = fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

-- z :: Maybe Integer
-- z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

-- tupled :: Maybe (Integer, Integer)
-- tupled = liftA2 (,) y z
-- tupled' =  (,) <$> y <*> z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int
maxed =liftA2 max' x y

xs = [1, 2, 3]
ys = [4, 5, 6]

xx :: Maybe Integer
xx = lookup 3 $ zip xs ys

yy :: Maybe Integer
yy = lookup 2 $ zip xs ys

-- summed :: Maybe Integer
summed = uncurry max <$> liftA2 (,) xx yy

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a) 

instance Applicative Identity where
  pure v = Identity v  
  (<*>) (Identity f) (Identity v)  = Identity (f v  )

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant { getConstant = mempty }
  (<*>) (Constant x) (Constant y) = Constant (mappend x y)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 10 s
mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 10 a

data Person =
  Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  case mkName n of
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of
        Nothing -> Nothing
        Just a' ->
          Just $ Person n' a'


maybeAddy = (mkAddress "old macs")

mkPersonBetter :: String -> String -> Maybe Person
mkPersonBetter n a =
  Person <$> mkName n <*> mkAddress a


work1 = const <$> Just "Hello" <*> pure "World"

-- "Tierness" [1, 2, 3]
work2A =  (,) <$> Just 90 <*> Just 10
--  map (\x -> pure x :: Maybe Int) [1,2,3]
work2B =  (,) <$>Just 10 <*> fmap (map(\x -> pure x :: Maybe Int)) (Just [1,2,3])

sameA = fmap (*2) [1..10]

-- see how pure lifts (*2) into the same structure we <*> over
sameB = pure (*2) <*> [1..10]
-- and here is manually lifting into the list structure
sameC = [(*2)] <*> [1..10]
-- heres another example using Maybe, same concept, pure lifting and manual lifting
moreA = pure (*3) <*> (Just 3)
moreB = Just (*3) <*> (Just 3)

data Listy a =
  Nil
  | Cons a (Listy a)
  deriving (Eq, Show)

instance Functor Listy where
  fmap f Nil = Nil 
  fmap f (Cons v (lst)) = Cons (f v) (fmap f lst)  

instance Applicative Listy where
  pure v = Cons v (Nil) 
  (<*>) Nil _ = Nil 
  (<*>) (Cons f ffs) (vals) = appendL (fmap f vals) (ffs <*> vals)


-- https://z0ltan.wordpress.com/2018/01/27/implementing-applicative-for-a-custom-list-type-in-haskell/
appendL Nil xs = xs 
appendL (Cons v xs ) ys = Cons v (appendL xs ys)


