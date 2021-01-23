import Control.Applicative
import Control.Monad
import Data.Char
boop = (*2)
doop = (+10)
-- bip :: Num a => a -> a 
bip = boop . doop
bipa = boop <$> doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)


cap :: [Char] -> [Char]
cap xs = map toUpper xs
rev :: [Char] -> [Char]
rev xs = reverse xs

composed = cap . rev 

fmappeda = fmap cap rev
fmappedb = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled x = (x, composed x)

tupleda :: [Char] -> ([Char], [Char])
tupleda xs = fmap composed (xs, xs) 


tupledd :: [Char] -> ([Char], [Char])
tupledd xs = join (,) (composed xs)

tupled' xs = (\ys zs -> [ys,zs]) <$> cap <*> rev

tupled'' = liftA2 (,) rev cap 

tupledm = do 
  a <- rev 
  b <- cap 
  return (a, b)

tupledm'= rev >>=
 \ reved ->
  cap >>=
   \ caped ->
    return (reved, caped)  

    
-- Functor of functions is function composition.
