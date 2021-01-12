{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DerivingVia #-}
-- import GHC.Generics
-- import Data.Semigroup.Generic

-- data Pair a = MkPair a a
--   deriving (Generic)
--   deriving (Semigroup, Monoid) via (GenericSemigroupMonoid (Pair a))
--   deriving (Show, Ord, Eq)

data List a =
  Nil
  | Cons a (List a)
  -- deriving (Eq, Show)
  -- deriving (Semigroup, Monoid) 
