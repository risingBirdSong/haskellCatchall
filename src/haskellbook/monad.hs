import Control.Monad
-- andOne x = [x, 1]

-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>) :: m a -> m b -> m b
-- return :: a -> m a


bind :: Monad m => (a -> m b) -> m a -> m b
bind f mon  = join $ fmap f mon 

-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a
