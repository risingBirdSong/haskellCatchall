data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)
  
take' :: Int -> List a -> List a
take' n lst  = go 0 lst 
  where go cnt (Nil) = Nil 
        go cnt (Cons val (subl)) 
            | cnt >= n = Nil
            | otherwise = Cons val (go (succ cnt) (subl))  

testLstA = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 4 Nil)))) 

-- how to recurse
lstMatch Nil = Nil 
lstMatch (Cons a (Nil)) = (Cons a Nil )  
lstMatch (Cons a (ls)) = lstMatch (ls)

instance Functor List where
  fmap = undefined
instance Applicative List where
  pure = undefined
  (<*>) = undefined