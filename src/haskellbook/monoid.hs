import Data.Monoid
import Test.QuickCheck


-- (Product 2) <> (Product 2) <> (Product 2)
-- mconcat [(Product 3)(Product 3)(Product 3)]

data Booly a =
  False'
  | True'
  deriving (Eq, Show)
-- conjunction; just cause.
-- instance Monoid (Booly a) where
--       mappend False' _ = False'
--       mappend _ False' = False'
--       mappend True' True' = True'

instance Semigroup a => Semigroup (Optional a) where
  Nada <> Nada = Nada 
  Nada <> (Only a) = Only a 
  Only a <> Nada = Only a 
  Only a <> Only b = Only (a <> b) 

instance Semigroup a => Monoid (Optional a) where
    mempty = Nada

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String
madlibbin' :: Exclamation
  -> Adverb
  -> Noun
  -> Adjective
  -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife "

madlibbinBetter' :: Exclamation
  -> Adverb
  -> Noun
  -> Adjective
  -> String
madlibbinBetter' e adv noun adj = mconcat [e, " he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife"]

type S = String
type B = Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


-- genFirstOptional :: (Arbitrary a) => Gen (First' (Optional a))
-- genFirstOptional = elements [First' (Only a), First' Nada]

data Optional a =
      Nada
      | Only a
      deriving (Eq, Show)

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)
 
instance Semigroup (First' a) where
   First' (Only a) <> First' (Only b) = First' (Only a)
   First' (Only a) <> First' Nada = First' (Only a)
   First' Nada <> First' (Only a) = First' (Only a)
   First' Nada <> First' Nada = First' Nada
instance Monoid (First' a) where 
  mempty = First' Nada

-- Instance Arbitrary (First' (a)) where 
--   arbitrary = do
--     a <- arbitrary 
--     frequency [(1, return (First' (Nada))) , (1, return ( First' (Only a)))]

instance Arbitrary a => Arbitrary (First' (a)) where  
  arbitrary = do
    a <- arbitrary
    frequency [(1, return (First' (Nada))) , (1, return ( First' (Only a)))]
  

firstMappend :: First' a
  -> First' a
  -> First' a
firstMappend = mappend

type FirstMappendTest =
  First' String
  -> First' String
  -> First' String
  -> Bool

type FstIdTest =
  First' String -> Bool

data NonEmpty a = a :| [a]
  deriving (Eq, Ord, Show)

ne :: NonEmpty Integer
ne =  1 :| [2,3]

-- huh so how to recurse with the NonEmpty list?
-- takeout :: (Num a, Ord a) => NonEmpty a -> a -> NonEmpty a 
-- takeout (xs) n
--   | n > 10 = xs 
--   | otherwise = takeout (  n :| xs) (n + 1)
-- takeout (x :| xs) = takeout (xs)

data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
 Trivial <> Trivial  = Trivial
instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- quickCheck  (semigroupAssoc  :: TrivialAssoc )
-- +++ OK, passed 100 tests.

newtype Identity a = Identity a deriving (Show, Eq, Ord)

instance Semigroup a => Semigroup (Identity a) where 
  Identity x <> Identity y = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where 
  arbitrary = do Identity <$> arbitrary

assocTestIdentity :: (Eq a, Semigroup a) => Identity a -> Identity a -> Identity a -> Bool
assocTestIdentity (Identity a) (Identity b) (Identity c) = ( Identity a <> Identity b) <> Identity c == Identity a <> ( Identity b <> Identity c)

assocCheckIdentityA = quickCheck (assocTestIdentity :: Identity [Int] -> Identity [Int] -> Identity [Int] ->  Bool)
assocCheckIdentityB = quickCheck (assocTestIdentity :: Identity String -> Identity String -> Identity String ->  Bool)
