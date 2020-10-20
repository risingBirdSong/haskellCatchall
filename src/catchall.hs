class C a where
  m                     :: Show b => a -> b

-- class C a where m :: Show b => a -> b you can read as
-- "If you want to write an instance of the type class C for any type a, you have to provide an implementation of m :: Show b => a -> b"
-- You can do that by writing, for example

-- Here's an example type constructor
data Pair a = MkPair a a
-- Oh let me be less ambiguous
-- Again, Pair is a type constructor
-- MkPair is not
-- It's a constructor
-- Not a type constructor
-- Big difference!!!


-- i did get taught what you wrote

