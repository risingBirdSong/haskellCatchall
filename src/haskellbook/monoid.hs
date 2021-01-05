import Data.Monoid
import Test.QuickCheck
import Debug.Trace


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

data Two a b = Two a b deriving (Show, Eq )

instance (Semigroup a , Semigroup b) => Semigroup (Two a b) where
  Two a b  <> Two aa bb = Two (a <> aa) (b <> bb)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where 
  arbitrary = do 
  a <- arbitrary
  b <- arbitrary 
  return (Two a b)

assocTestTwo :: (Eq a, Semigroup a) => Two a a -> Two a a -> Two a a -> Bool 
assocTestTwo (Two a b) (Two aa bb) (Two aaa bbb) = (Two a b <> Two aa bb) <> Two aaa bbb  == Two a b <> (Two aa bb <> Two aaa bbb)
assocCheckTwo = quickCheck (assocTestTwo :: Two [Int] [Int] -> Two [Int] [Int] -> Two [Int] [Int] -> Bool)

assocTwoA = (Two [1] [1] <> Two [2] [2]) <> Two [3] [3] 
assocTwoB = Two [1] [1] <> (Two [2] [2] <> Two [3] [3] )

-- since the smallest constituent part of Two, a <> aa (and b <> bb) is a Semigroup then Two itself is also a Semigroup, because it is just a container for two Semigroups, (self similar / recursive) and it combines its parts according to Semigroup law. a binary operation that is associative. 

-- -> (definition of <> for Two)
-- Two (a <> aa) (b <> bb) <> Two aaa bbb == Two a b <> Two (aa <> aaa) (bb <> bbb

stepA =(Two a b <> Two aa bb) <> Two aaa bbb == Two a b <> (Two aa bb <> Two aaa bbb)
stepB = (Two (a <> aa) (b <> bb)) <> Two aaa bbb == Two a b <> (Two (aa <> aaa) (bb <> bbb))
stepC = Two (a <> aa <> aaa) (b <> bb <> bbb) == Two (a <> aa <> aaa) (b <> bb <> bbb)

a = [1]
b = [1,1]
aa = [2] 
bb = [2,2]
aaa = [3]
bbb = [3,3]

-- (1 + 2) + 3 = 6 == 1 + (2 + 3)

data Three a b c = Three a b c deriving (Show, Eq)
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where 
  Three a b c <> Three aa bb cc = (Three (a <> aa)(b <> bb)(c <> cc))  

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where 
  arbitrary = do 
    a <- arbitrary 
    b <- arbitrary 
    c <- arbitrary 
    return (Three a b c)

threeTest (Three a b c) (Three aa bb cc) (Three aaa bbb ccc) = Three a b c <> (Three aa bb cc <> Three aaa bbb ccc) == (Three a b c <> Three aa bb cc) <> Three aaa bbb ccc
threeCheck = quickCheck (threeTest :: Three [Int] [Int] [Int] -> Three [Int] [Int] [Int] -> Three [Int] [Int] [Int] -> Bool)

a1 = ["ab", "c"]
a2 = ["a", "bc"]

-- js :: (Foldable t, Eq a) => t a -> [a] -> Int
js j s = length $ filter (\x -> x `elem` j) s


newtype BoolConj =
  BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True 
  BoolConj True <> BoolConj False = BoolConj False 
  BoolConj False <> BoolConj False = BoolConj False 
  BoolConj False <> BoolConj True = BoolConj False 

instance Arbitrary BoolConj where 
  arbitrary = do BoolConj <$> arbitrary


boolConjTest (BoolConj a) (BoolConj b) (BoolConj c) = BoolConj a <> (BoolConj b <> BoolConj c) == (BoolConj a <> BoolConj b) <> BoolConj c  
boolConjCheck = quickCheck (boolConjTest :: BoolConj -> BoolConj -> BoolConj -> Bool)
instance Semigroup Bool where 
  True <> True = True 
  _ <> _  = False 

newtype BoolDisj =
  BoolDisj Bool deriving (Show, Eq)

instance Semigroup BoolDisj where 
  BoolDisj True <> BoolDisj True = BoolDisj True 
  BoolDisj True <> BoolDisj False = BoolDisj True 
  BoolDisj False  <> BoolDisj True = BoolDisj True 
  _ <> _  = BoolDisj False 

instance Arbitrary BoolDisj where
  arbitrary = do BoolDisj <$> arbitrary 

boolDisjTest (BoolDisj a)(BoolDisj b)(BoolDisj c) = BoolDisj a <> (BoolDisj b <> BoolDisj c) == (BoolDisj a <> BoolDisj b) <> BoolDisj c 
boolDisjCheck = quickCheck boolDisjTest

data Or a b = 
  Fst a | Snd b deriving (Show, Eq)

instance Semigroup (Or a b ) where 
  Snd x <> _ = Snd x 
  _ <> Snd y = Snd y 
  Fst x <> Fst y = Fst y 


-- Not in scope: type constructor or class ‘Fst’
-- instance (Arbitrary a) => Arbitrary (Fst a) where 
--   arbitrary = do
--   a <- arbitrary 
--   return (Fst a)
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where 
  arbitrary = do
  a <- arbitrary 
  b <- arbitrary 
  oneof [return (Fst a), return (Snd b)] 

-- Not in scope: data constructor ‘Or’
-- orTest (Or a aa)(Or b bb)(Or c cc) = (Or a aa) <> ((Or b bb) <> (Or c cc)) == (Or a aa <> Or b bb) <> Or c cc 

-- orTestA :: (Eq a, Semigroup a) => Or a a-> Or a a-> Or a a-> Bool 
genericAssocTest a b c = (a <> ( b <> c)) == (( a <>  b) <>  c)
orCheck = quickCheck (genericAssocTest :: Or String Int -> Or String Int -> Or String Int -> Bool)
orA = Snd 1 <> (Snd 2 <> Fst 3)  == (Snd 1 <> Snd 2) <> Fst 3
orB = Fst 1 <> (Snd 2 <> Fst 3) == (Fst 1 <> Snd 2) <> Fst 3

-- orTest :: Gen (Or Bool Bool)
-- orTest = arbitrary 

newtype Combine a b =
  Combine { unCombine :: (a -> b) } 

instance Semigroup b => Semigroup (Combine a b) where 
  Combine f <> Combine g = Combine (\x -> f x <> g x )

-- my first attempt
-- instance Semigroup b => Semigroup (Combine a b) where 
--   Combine aa <> Combine bb = Combine (aa <> bb)

f = Combine $ \n -> Sum (n + 1) 
g = Combine $ \n -> Sum (n - 1)
uncombineTest = unCombine (f <> g) $ 0

ff = \n -> Sum (n + 1)
gg = \n -> Sum (n - 1)

hh = ff <> gg
