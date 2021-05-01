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

