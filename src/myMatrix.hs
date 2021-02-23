import Data.List 
import Data.Char
myMatrix = [
    [1,3,2,4,5],
    [6,7,9,2,3],
    [9,8,4,5,6],
    [0,4,1,7,8]
  ]
diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
    -- it is critical for some applications that we start producing answers
    -- before inspecting es_
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]

diagonals' []       = []
diagonals' ([]:xss) = xss
diagonals' xss      = zipWith (++) (map ((:[]) . head) xss ++ repeat [])
                                  ([]:(diagonals' (map tail xss)))


diagonals'' = map concat
          . transpose
          . zipWith (\ns xs -> ns ++ map (:[]) xs)
                    (iterate ([]:) [])

-- [ [1,2,3], [4,5,6], [7,8,9] ]


headTest xs = [h | h:_ <- xs]
tailTest xs = [t | _:t <- xs]