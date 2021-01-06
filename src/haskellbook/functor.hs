-- fmap (+1) (Just 1)
-- Just 2

-- *Main> fmap (fmap (+1)) [(Just 1),(Just 2),Nothing]
-- [Just 2,Just 3,Nothing]

class Sumthin a where
  s :: a -> a

class Else b where
  e :: b -> f (g a b c)

data T b = T b  

class Something a where 
  somefunction :: a -> b a -> T (b a)
  
data Wrapper a = Wrapper a

data Star = Star

-- data FixMePls =
--   FixMe
--   | Pls
--   deriving (Eq, Show)

-- instance Functor FixMePls where
--   fmap = error "it doesn't matter, it won't compile"

--     * Expected kind `* -> *', but `FixMePls' has kind `*'
--     * In the first argument of `Functor', namely `FixMePls'
--       In the instance declaration for `Functor FixMePls'
--    |
-- 27 | instance Functor FixMePls where

data FixMePls a =
  FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

-- In Haskell, the two use cases have been merged such that we
-- don’t call out constructor classes as being separate from typeclasses,
-- but we think it’s useful to highlight that something significant has
-- happened here. Now we have a means of talking about the contents
-- of types independently from the type that structures those contents.
-- That’s why we can have something like fmap that allows us to alter
-- the contents of a value without altering the structure (a list, or a Just)
-- around the value

-- you can't talk about Functor f without higher kinded types
-- f has to be something accepting one more parameter
-- without higher kinded types
-- like you can't even express this polymorphism