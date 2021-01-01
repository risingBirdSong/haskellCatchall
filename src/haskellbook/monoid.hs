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
