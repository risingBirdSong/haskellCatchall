import Data.Foldable
import Data.Monoid

foldalla = foldMap All [True, False, True]
-- All {getAll = False}
foldallb = foldMap All [True, True , True]
-- All {getAll = True}

mysum :: (Foldable t, Num a) => t a -> a
mysum fld = getSum (foldMap Sum $ fld)

myproduct :: (Foldable t, Num a) => t a -> a
myproduct d = getProduct (foldMap Product d)

myelem :: (Foldable t, Eq a) => a -> t a -> Bool
myelem = any . (==)

-- myminimum xs = foldr (min) (1/0) xs
-- 9223372036854775807 is the highest max bound for Int
-- myminimum xs = foldr (min) (9223372036854775807) xs
myminimum [] = Nothing
myminimum xs = Just (foldr (min) (head xs))

-- the official implementation using a newtype and coercible, found in utilities
-- minimum = fromMaybe (errorWithoutStackTrace "minimum: empty structure") .
--     getMin . foldMap (Min #. (Just :: a -> Maybe a))

mymaximum [] = Nothing
mymaximum xs = Just (foldr max 0 xs)

-- mynull :: (Foldable t, Eq (t a), Monoid (t a)) => t a -> Bool
-- mynull xs 
--   | xs == mempty = True
--   | otherwise = False
snull :: (Foldable t) => t a -> Bool
snull = foldr (\_ _ -> True) False
mynull :: (Foldable t) => t a -> Bool
mynull = foldr (\_ _ -> False) True

mylength :: (Foldable t) => t a -> Int
mylength xs = foldr (\_ ac -> succ ac) 0 xs

myToList :: (Foldable t) => t a -> [a]
myToList xs = foldr (\v ac -> v : ac) [] xs

myfold :: (Foldable t, Monoid m) => t m -> m
myfold xs = foldr (<>) mempty xs

refold f xs = foldMap f xs

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

myconvert :: (Foldable t , Functor t, Monoid m) => (a -> m) -> t a -> t m
myconvert f xs = fmap f xs
myFoldMap :: (Foldable t, Functor t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f xs = foldr (<>) mempty $ fmap f xs


foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\b a -> (f b) <> a) mempty

data Two a b =
  Two a b

instance Foldable (Two a) where 
  foldMap f (Two _ bb) = f bb

data Three a b c =
  Three a b c

instance Foldable (Three a b) where 
  foldMap f (Three a b c) = f c

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where 
  foldMap f (Three' a b bb) = (f b) <> (f bb)


data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b bb bbb) = (f b)<>(f bb)<>(f bbb)


