

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
instance Eq (TisAnInteger) where 
    (==) (TisAn n) (TisAn n') = n == n'

    