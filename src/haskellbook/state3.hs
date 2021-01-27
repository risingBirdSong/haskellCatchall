{-# LANGUAGE InstanceSigs #-}

import System.Random

newtype State s a = 
  State {runState :: s -> (a,s)}

instance Functor (State s) where 
  fmap :: (a-> b) -> State s a -> State s b
  fmap f (State g) = State (\s -> (\(a,s') -> (f a , s')) $ g s ) 