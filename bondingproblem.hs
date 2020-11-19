-- 1sec
-- problem: Given a binary predicate P on a set S (that is, a subset of S x S) we say that a function B from S to itself is a Bonding if it is total, symmetric, and, for all x,y in S, B(x) = y implies that y does not equal x and furthermore P(x,y) holds. In other words, each element in S is paired with exactly one other element such that the pairs satisfy the predicate.

-- In this exercise you will need to write a function that finds Bondings. We will model the input predicate P in Haskell as a function of type (a -> a -> Bool). We will model the set S as a list of type [a]. As Bondings do not always exist for a given P and S, we will return a Bonding as a Maybe type of a list of pairs [(a,a)].

-- Define a function

-- findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe [(a,a)]
-- such that findBonding p xs returns any Bonding of the list xs for predicate p. You may assume that xs does not contain duplicate elements.
-- For example, findBonding (\x -> \y -> odd(x+y)) [2,3,4,5,6,7] may return

-- Just [(2,3),(3,2),(4,5),(5,4),(6,7),(7,6)]
-- as a valid answer. Whereas findBonding (\x -> \y -> even(x+y)) [2,3,4,5,6,7] should return Nothing as no Bonding exists in this case.