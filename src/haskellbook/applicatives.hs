import Control.Applicative
import Data.List

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
