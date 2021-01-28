{-# LANGUAGE InstanceSigs #-}

import System.Random
import Control.Monad

newtype State s a = 
  State {runState :: s -> (a,s)}

instance Functor (State s) where 
  fmap :: (a-> b) -> State s a -> State s b
  -- fmap f (State g) = State (\s -> (\(a,s') -> (f a , s')) $ g s ) 
  fmap f (State g) = State $ \s ->
    let (a, s') = g s 
    in (f a, s') 

-- *Main> runState ((+1) <$> (State  $ \s -> (0,s))) 0
-- (1,0)

-- (<*>) :: State s (a -> b)
-- -> State s a
-- -> State s b
instance Applicative (State s) where 
  pure x = State (\s -> (x, s))
  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) (State sab) (State sa) = State $ \s ->
    let (f, s') = sab s
        (a, s'') = sa s' 
    in (f a, s'') 
    

-- incState :: State Int Int
incState = State $ \s -> (s + 1, s + 1)

ran = runState ((,,,) <$>  incState <*> incState <*> incState <*> incState) 0

  -- (>>=) :: State s a
  --   -> (a -> State s b)
  --   -> State s b
instance Monad (State s) where
  return = pure
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State f) >>= g = State $ \s ->
      let (a, s') = f s 
          ssb = g a 
      in runState ssb s' 

      -- pattern matching with State, is like what runState is doing, simply unwrapping
      -- let (a, s') = f s
      --   State ssb = g a 
      --   (b, s'') = ssb s'
      -- in (b, s'') 

-- Yeah, I specifically chose a calculation that required Monad instead of Applicative, since the value of y is dependant on x, the result of incState. You need Monad for that.
statemonadexample =  runState (do { x <- incState; y <- replicateM x incState; return (x, y) }) 5


-- *Main> statemonadexample
-- ((6,[7,8,9,10,11,12]),12)

get :: State s s
get = State $ (,) <$> id <*> id

get_  = (,) <$> id <*> id 
geta  = (,) <$> id 

put :: s -> State s ()
-- put s = State $ (,) <$> mempty  <*> id
put s = State $ \x -> ((), s)
-- Prelude> runState (put "blah") "woot"
-- ((),"blah")
