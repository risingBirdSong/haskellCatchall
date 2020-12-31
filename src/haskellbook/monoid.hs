import Data.Monoid

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


data Optional a =
      Nada
      | Only a
      deriving (Eq, Show)


instance Semigroup a => Semigroup (Optional a) where
  Nada <> Nada = Nada 
  Nada <> (Only a) = Only a 
  Only a <> Nada = Only a 
  Only a <> Only b = Only (a <> b) 

instance Semigroup a => Monoid (Optional a) where
    mempty = Nada

