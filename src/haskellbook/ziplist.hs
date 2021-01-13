import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where 
  (<>) vs Nil = vs
  (<>) Nil vs = vs
  (<>) (Cons v vs) (Cons x xs) = Cons v (Cons x (vs <> xs)) 

instance Monoid (List a) where 
  mempty = Nil

instance (Eq a) => EqProp (List a) where (=-=) = eq

genList :: Arbitrary a => Gen (List a)
genList = do 
  x <- arbitrary
  lst <- genList
  frequency [ (1, return Nil)
            , (5, return (Cons x lst)) ]

instance Arbitrary x => Arbitrary (List x) where 
  arbitrary = genList

take' :: Int -> List a -> List a
take' n lst  = go 0 lst 
  where go cnt (Nil) = Nil 
        go cnt (Cons val (subl)) 
            | cnt >= n = Nil
            | otherwise = Cons val (go (succ cnt) (subl))  

testLstA = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))) 
testLstB = Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil)))) 

-- how to recurse
lstMatch Nil = Nil 
lstMatch (Cons a (Nil)) = (Cons a Nil )  
lstMatch (Cons a (ls)) = lstMatch (ls)

instance Functor List where
  fmap f Nil = Nil  
  fmap f (Cons v (ls)) = Cons (f v) (fmap f ls)  
instance Applicative List where
  pure v = Cons v Nil 
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) (Cons b bs) = Cons (f b) (fs <*> bs)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

ziptA = Cons (+1) (Cons (*5) (Cons (*10) Nil))
ziptB = Cons (1) (Cons (5) (Cons (10) Nil))

-- z = ZipList' [(+9), (*2), (+8)]
-- z' = ZipList' [1..3]

-- *Main> ziptA <*> ziptB
-- Cons 2 (Cons 25 (Cons 100 Nil))


lstToZipF = [(+9), (*2), (+8)]
lstToZipD = [1..3]

toStd (Cons x xs) = x : toStd xs
toStd Nil = []

fromStd (x:xs) = Cons x (fromStd xs)
fromStd [] = Nil

-- *Main> fromStd [1,2,3]
-- Cons 1 (Cons 2 (Cons 3 Nil))
-- *Main> toStd (Cons 1 (Cons 2 (Cons 3 Nil)))
-- [1,2,3]