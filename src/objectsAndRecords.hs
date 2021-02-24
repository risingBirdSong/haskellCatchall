data X = X { str :: String, int :: Int } deriving Show 
v = X { str = "hello", int = 2 }

-- e :: X 
fff :: String -> X
fff input = X { str = input , int = 2}

-- a basic way to update an object 
updater :: String -> X -> X; 
updater input x = x { str = input }

