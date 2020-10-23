-- Problem 54A
-- (*) Check whether a given term represents a binary tree

-- In Prolog or Lisp, one writes a predicate to do this.

-- Example in Lisp:

-- * (istree (a (b nil nil) nil))
-- T
-- * (istree (a (b nil nil)))
-- NIL
-- Non-solution:

-- Haskell's type system ensures that all terms of type Tree a are binary trees: it is just not possible to construct an invalid tree with this type. Hence, it is redundant to introduce a predicate to check this property: it would always return True.

-- qoutRem
-- (integer division, remainder)
qrt = quotRem 5 5 -- (1,0)
qrta = quotRem 7 5 -- (1,2)
qrtb = quotRem 10 5 -- (2, 0)
qrtc = quotRem 12 5 -- (2, 2)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

-- cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2
    in [Branch i left right | i     <- [q .. q + r],
                                left  <- cbalTree i,
                                right <- cbalTree (n - i - 1)]