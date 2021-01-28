{-# LANGUAGE InstanceSigs #-}

import System.Random

newtype State s a = 
  State {runState :: s -> (a,s)}

instance Functor (State s) where 
  fmap :: (a-> b) -> State s a -> State s b
  fmap f (State g) = State (\s -> (\(a,s') -> (f a , s')) $ g s ) 

-- *Main> runState ((+1) <$> (State  $ \s -> (0,s))) 0
-- (1,0)

-- (<*>) :: State s (a -> b)
-- -> State s a
-- -> State s b
instance Applicative (State s) where 
  pure x = State (\s -> (x, s))
  (<*>) (State sab) (State sa) = State (\s ->
    let (a, s') = sa s 
        (f, s'') = sab s'
    in (f a, s'') 
    )