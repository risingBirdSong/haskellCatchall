{-# LANGUAGE InstanceSigs #-}
import Control.Applicative
import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

boop = (*2)
doop = (+10)
-- bip :: Num a => a -> a 
bip = boop . doop
bipa = boop <$> doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)


cap :: [Char] -> [Char]
cap xs = map toUpper xs
rev :: [Char] -> [Char]
rev xs = reverse xs

composed = cap . rev 

fmappeda = fmap cap rev
fmappedb = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled x = (x, composed x)

tupleda :: [Char] -> ([Char], [Char])
tupleda xs = fmap composed (xs, xs) 


tupledd :: [Char] -> ([Char], [Char])
tupledd xs = join (,) (composed xs)

tupled' xs = (\ys zs -> [ys,zs]) <$> cap <*> rev

tupled'' = liftA2 (,) rev cap 

tupledm = do 
  a <- rev 
  b <- cap 
  return (a, b)

tupledm'= rev >>=
 \ reved ->
  cap >>=
   \ caped ->
    return (reved, caped)  

    
-- Functor of functions is function composition.

newtype Reader r a =
  Reader { runReader :: r -> a } 

ask :: Reader a a
ask = Reader id   

-- *Main> runReader ask 3
-- 3

newtype HumanName =
  HumanName String
  deriving (Eq, Show)
newtype DogName =
  DogName String
  deriving (Eq, Show)
newtype Address =
  Address String
  deriving (Eq, Show)
data Person =
  Person {
  humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)
data Dog =
  Dog {
  dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

chris :: Person
chris = Person (HumanName "Chris Allen")
  (DogName "Papu")
  (Address "Austin")


getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address

-- *Main> getDog chris
-- Dog {dogsName = DogName "Papu", dogsAddress = Address "Austin"}
-- *Main> getDogR chris
-- Dog {dogsName = DogName "Papu", dogsAddress = Address "Austin"}

myLiftA2 :: Applicative f =>
  (a -> b -> c)
  -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

-- and a way to handle a tagged union as well 


-- data ListenExpiried = -- stuff
-- instance FromJSON ListenExpiried where
--   -- decode from the thing that's supposed to be in that 'data' field

-- -- more types and instances, similar to the above

-- data Event = EListenExpiried ListenExpiried | -- other events

-- instance FromJSON Event where
--   parseJSON = withObject "event" $ \o -> do
--     tag <- o .: "e"
--     payload <- o .: "data"
--     case tag of
--       "listenexpiried" -> ListenExpiried <$> parseJSON payload
--       "account_update" -> AccountUpdate <$> parseJSON payload

asks :: (r -> a) -> Reader r a
asks = Reader

-- fmap :: (a -> b)
-- -> (r -> a)
-- -> (r -> b)
instance Functor (Reader r) where 
  fmap f (Reader r) = Reader (\a -> f (r a))

-- <*> :: (r -> a -> b)
-- -> (r -> a)
-- -> (r -> b)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader (\r' -> a)
  (<*>) (Reader rab) (Reader ra) = Reader (\r -> rab r (ra r) )


instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a
    -> (a -> Reader r b)
    -> Reader r b
  (Reader ra) >>= aRb = join $ Reader $ \r -> aRb $ ra r

-- Reader $ \r -> undefined

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]


-- xs :: Maybe Integer
xs = lookup 3 $ zip x y 

ys :: Maybe Integer
ys = lookup 6 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x y

x1 :: Maybe (Integer, Integer)
x1 =  (,) <$> xs <*> xs 
x1' :: Maybe (Integer, Integer)
x1' =  (,) <$> xs <*> ys 

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> xs


summed :: Num c => (c, c) -> c
summed = uncurry (+)
