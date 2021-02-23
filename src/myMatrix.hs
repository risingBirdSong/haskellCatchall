import Data.List 
import Data.Char
import Debug.Trace
testMatrix = [
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
diagonals' xss      =  zipWith (++) (trace ("leftBranch>" ++ (take 20 $ show leftBranch))leftBranch)
                                  (trace ("rightBranch>" ++ show rightBranch) rightBranch )
                      where leftBranch = map (\x -> (:[]) $ head x) xss ++ repeat []
                            rightBranch = ([]:(diagonals' (map tail xss)))

-- *Main> diagonals' testMatrix
-- leftBranch>[[1],[6],[9],[0],[],
-- leftBranch>[[3],[7],[8],[4],[],
-- leftBranch>[[2],[9],[4],[1],[],
-- leftBranch>[[4],[2],[5],[7],[],
-- leftBranch>[[5],[3],[6],[8],[],
-- rightBranch>[[],[],[],[]]
-- rightBranch>[[],[5],[3],[6],[8]]
-- rightBranch>[[],[4],[2,5],[5,3],[7,6],[8]]
-- rightBranch>[[],[2],[9,4],[4,2,5],[1,5,3],[7,6],[8]]
-- rightBranch>[[],[3],[7,2],[8,9,4],[4,4,2,5],[1,5,3],[7,6],[8]]
-- [[1],[6,3],[9,7,2],[0,8,9,4],[4,4,2,5],[1,5,3],[7,6],[8]]

diagonals'' = map concat
          . transpose
          . zipWith (\ns xs -> ns ++ map (:[]) xs)
                    (iterate ([]:) [])

-- [ [1,2,3], [4,5,6], [7,8,9] ]


headTest xs = [h | h:_ <- xs]
tailTest xs = [t | _:t <- xs]