import Data.List 
import Data.Char
import Debug.Trace

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

-- leftBranch>[[1],[6],[11],[16],[
-- leftBranch>[[2],[7],[12],[17],[
-- leftBranch>[[3],[8],[13],[18],[
-- leftBranch>[[4],[9],[14],[19],[
-- leftBranch>[[5],[10],[15],[20],
-- rightBranch>[[],[],[],[]]
-- rightBranch>[[],[5],[10],[15],[20]]
-- rightBranch>[[],[4],[9,5],[14,10],[19,15],[20]]
-- rightBranch>[[],[3],[8,4],[13,9,5],[18,14,10],[19,15],[20]]
-- rightBranch>[[],[2],[7,3],[12,8,4],[17,13,9,5],[18,14,10],[19,15],[20]]
-- [[1],[6,2],[11,7,3],[16,12,8,4],[17,13,9,5],[18,14,10],[19,15],[20]]
testMatrix = [
  [1, 2, 3],
  [4, 5, 6],
  [7, 8, 9]
  ]

diagonals'' :: (Num a, Show a) => [[a]] -> [[a]]
diagonals'' = map concat
          . transpose
          . (\x -> trace (show x) x)
          . zipWith (\ns xs -> (ns ++ map (:[]) xs))
                    (iterate ([]:) [])



remakeDiagonals xss = map concat . transpose $ zipWith (\blanks xs -> blanks ++ (map (:[]) xs)) (iterate ([]:) []) xss
