import Data.Foldable
import Data.Monoid

foldalla = foldMap All [True, False, True]
-- All {getAll = False}
foldallb = foldMap All [True, True , True]
-- All {getAll = True}

mysum :: (Foldable t, Num a) => t a -> a
mysum fld = getSum (foldMap Sum $ fld)
  