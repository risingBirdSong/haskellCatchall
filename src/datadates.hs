

f :: Int -> Bool
f 1 = True
f 2 = True
f 9 = True
f _ = False

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving (Show)

-- https://stackoverflow.com/questions/28125038/no-explict-implementation-warning/28125833
instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

data Date = Date DayOfWeek Int deriving (Eq, Show)

newtype Identity a = Identity a deriving (Show)

-- the following wont work, (==) won't work by defauly since a is an unknown type
-- and therefore Haskell wont assume that its eqautable
-- constraining it like the next example works.
-- instance Eq (Identity a) where
--   (==) (Identity v) (Identity v') = v == v'
instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

newtype TisAnInteger = TisAn Integer deriving (Show)


-- cool i wasn't sure, but it works :) 
-- the Eq instance is on the TisAnInteger type constructor
-- the int n parameterized with data constructor Tisan supplies the equality check
-- and this is possible because GHCI knows TisAn n is an Integer from where it was defined
instance Eq TisAnInteger where 
    (==) (TisAn n) (TisAn n') = n == n'

    
data TwoIntegers = Two Integer Integer deriving (Show)
instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = a == a' && b == b'

data StringOrInt =
  TisAnInt Int
  | TisAString String

instance Eq StringOrInt where 
   (TisAnInt n) == (TisAnInt n') = n == n' 
   (TisAString s) == (TisAString s') = s == s' 
   _ == _ = False

data Pair a =
  Pair a a deriving Show

instance Eq a => Eq (Pair a) where 
  (Pair a b) == (Pair a' b') = a == a' && b == b' 

data MyTuple a b =
  MyTuple a b

instance (Eq a, Eq b) => Eq (MyTuple a b) where 
  (MyTuple a b) == (MyTuple a' b') = a == a' && b == b'

data Which a =
  ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where 
  (ThisOne a) == (ThisOne a') = a == a'
  (ThatOne a) == (ThatOne a') = a == a'
  (ThisOne a) == (ThatOne a') = a == a'
  (ThatOne a) == (ThisOne a') = a == a'