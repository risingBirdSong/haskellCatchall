f :: Int -> String
f = undefined
g :: String -> Char
g = undefined
h :: Int -> Char
h a = g $ f a 


munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToY yToWZ xx = fst $ yToWZ $ xToY xx 

