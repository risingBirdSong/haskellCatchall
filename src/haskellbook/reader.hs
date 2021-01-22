import Control.Applicative
boop = (*2)
doop = (+10)
-- bip :: Num a => a -> a 
bip = boop . doop
bipa = boop <$> doop
