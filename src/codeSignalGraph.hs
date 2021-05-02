import Data.List 
import qualified Data.Set as S

-- https://app.codesignal.com/arcade/graphs-arcade/kingdom-roads/nCMisf4ZKpDLdHevE

-- newRoadSystem roadRegister = 

mtrxA =[[False, True,  False, False],
        [False, False, True,  False],
        [True,  False, False, True ],
        [False, False, True,  False]] 


allTrues boolList = filter (\x -> x == True) boolList

newRoadSystem mtrx = all (\b -> b == True) $ zipWith (\ a b -> allTrues a == allTrues b ) mtrx (transpose mtrx)


roadsBuilding cities roads = S.toList $ S.difference ( S.fromList [ sort [x,y]  | x <- [0..cities-1] , y <- [0..cities-1], x /= y ] ) (roadSet)
    where roadSet = S.fromList (doubleSort roads)

doubleSort xxs = sort $ map sort xxs

-- https://app.codesignal.com/arcade/graphs-arcade/kingdom-roads/ty4w8WJZ4sZSBNK5Q

roads = [[3, 0], [0, 4], [5, 0], [2, 1],
          [1, 4], [2, 3], [5, 2]]
roadsA = [[0, 4], [5, 0], [2, 1],
          [1, 4], [2, 3], [5, 2]]

efficientRoadNetwork n roads = all (==True) $ [ doRoadsConnect x y roads | x <- [0..n-1] , y <- [0..n-1] , x /= y ]

connections num roads = concat $ filter (num `elem`) roads

doRoadsConnect str end roads = not . null $ connections str roads `intersect` connections end roads