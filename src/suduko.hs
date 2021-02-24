-- from Graham Hutton's lecture and code
-- https://www.cs.nott.ac.uk/~pszgmh/sudoku.lhs
--https://www.youtube.com/watch?v=glog9DZh8G0&ab_channel=GrahamHutton

import Data.List

type Grid             = Matrix Value
type Matrix a         = [Row a]
type Row a            = [a]
type Value            = Char

boxsize               :: Int
boxsize               =  3

blank                 :: Grid
blank                 =  replicate n (replicate n '.')
                         where n = boxsize ^ 2

rows                  :: Matrix a -> [Row a]
rows                  =  id

cols                  :: Matrix a -> [Row a]
cols                  =  transpose

boxs                  :: Matrix a -> [Row a]
boxs                  =  unpack . map cols . pack
                         where
                             pack   = split . map split
                             split  = chop boxsize
                             unpack = map concat . concat

chop                  :: Int -> [a] -> [[a]]
chop n []             =  []
chop n xs             =  take n xs : chop n (drop n xs)

examplematrix = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
examplematrixA = [[1,2,3,4,5,6],[7,8,9,10,11,12],[13,14,15,16,17,18]]
-- *Main> boxs examplematrixA (note with box size set to three)
-- [[1,2,3,7,8,9,13,14,15],[4,5,6,10,11,12,16,17,18]]

-- Example: if boxsize = 2, then we have 

--    [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]

--                           |
--                          pack
--                           |
--                           v

--    [[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]

--                           |
--                        map cols
--                           |
--                           v

--    [[[[1,2],[5,6]],[[3,4],[7,8]]],[[[9,10],[13,14]],[[11,12],[15,16]]]]

--                           | 
--                         unpack
--                           |
--                           v

--    [[1,2,5,6],[3,4,7,8],[9,10,13,14],[11,12,15,16]]

-- Note that concat . split = id, and moreover, boxs . boxs = id.
