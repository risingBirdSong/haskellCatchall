import Data.List.Split
oneCol str = drop 1 $ init $ splitOn "" str

lengthOfWhatIShouldNeed str = 3 * (length $ drop 1 $ init $ splitOn "" str)
--90. which matches my mental math

main = do 
  taking <- lines <$> readFile "adventofcode\\day3data.txt"
  print $ length $ taking
  -- length of 323 which is way more than needed
  let needed = take 95 taking
  
  print needed 
  return ()



working = ["..#..#......#..#.......#...#.#.","...##.....##..#..#....#.##.##.#","...#...#.##...##.....#.....#.#.","..#....#.....#...##.##.###.#...",".#.....#......#..#.##.#...###..","#..#..#.#......#...........###.","#......####.#....##........##..",".#.......#.....#......#...#....","...#...#.....#.......##.....##.","#...##.........#.#..##..#..#.##","#.#.##.........#.#..#.#.....###",".##..##...#....##.....#..#.....","........#.......###.#.#.....#.#","...#.#....#.##..#...##..##..#..","......#....#........######.#...",".##...#.#...###......#.#.#..#.#","........#.##...##.#...#..#...##",".#..#.#..##....###..#.#.......#","..#..##..#.#...#.##......#.....","##.....#..##.#.#..#......##...#","......................#..#..#..","..#.##....####.........###.##..","##..###..#...#....#..#.#...#...",".##.#......#..#....#........#..",".#.....#..#..#.#.#....#.....##.","..........#..#....#..##...#..##",".#...#.#....#.##..#.....#....#.","#..............#.#.#..#..#....#","...#.#...............##........","#.#.##...#.##..##.....#........","...#.......###..###..#...#..#..","####..#.#..##.....##.#.#......#",".#.#.......#..##.......#.......","#....#...#.##.#.......#..#.....",".#...##..#..#..##.......##...#.",".#..#......#.........#.........","#.##.#.....#....#..##..#.....#.","#.#....#.#....#...#.#..#....#..","#..#.....#.##..#.....#...##...#","#....#...##.#.........#.#....##",".......##.##......##.......##..","#.....#..#........#........#...","#....#.#..#.#........##.#...#..","#.......#.#.#.#....#.......##.#","...#..###..........#...#.#.###.","....#..#....#...#....##.#.....#",".#..##.....#..#....##..##...#.#","#.........#....#.#..###...##...",".#.#.........#.#.......#.#.#..#","..........#........##..#.......",".....#.......#...#.....#..##.##","...#.........#.............####","##..#...#..#.#......#...#......",".#..###...#.#.#.#...#...#......","....#..##.#....#..#.#..##..##.#","..#.......#......#..#.......#..","....###......#...##...#....#...","..#..#.....#...#..###....#.#..#",".........##..#.##....#..##..#..","##...#...#.#.........##......#.","###..#.#....#......##..##.#...#",".##...##..#.#.#.#......#..#....","###......#..#..#.....#..#....#.",".#.#..##....##........##..#.#..","###...####.#....#.......###....","..#....###..#.#.#..#.......##..",".......#.#...#.....#.#....##.#.","......#......#.#....#..##..###.","....####..........#.....#......",".###.....#...#..#...##.#...###.","...##....##....###....#.#..#.#.","##.#..........##.........#.##..","..#..#.#.###..##..#....##.....#","..#....##.....#...##....###..##","....#.......##..#..#..........#","............#..#.###..#.#......","...........##......#.#.#...#..#","...##.##....#...##.##.....#.#..",".####...#....###...#.....#....#",".##........#..##..#.#.....#....","..................#.....#..##..","..###.....#.##..#..#....##...#.","...#.##.#.####.#.###.#....#..##",".#....##..##......####.#####...","#...#.#....##.........##....#..","..#.##.....##.............#.##.","###.....#.#..#..#......#.##.#..","...#..##.....#...##...#......#.",".##.#...#......##.#..##....#...",".....##.....#......#.#........."]
aa = (!!) working 0 
bb = (!!) working 1 
cc = (!!) working 2 
dd = (!!) working 3 

slopeMatrix mtr x y = (!!) ((!!) mtr x) y 

countTrees mtr = countTrees' mtr 0 0 0 
countTrees' mtr x y count 
  | x >= 90 || y >= 30 = count
  | slopeMatrix mtr x y == '#' = countTrees' mtr (x + 1) (y + 3) (count + 1)  
  | slopeMatrix mtr x y == '.' = countTrees' mtr (x + 1) (y + 3) (count + 0) 

aStr = "..#..#......#..#.......#...#.#."
discoveryStr str x 
  | (!!) str x == '#' = (take x str) ++ "T" ++ (drop (x + 1) str)
  | (!!) str x == '.' = (take x str) ++ "O" ++ (drop (x + 1) str)

treeFinder mtr y x nwMtr cnt 
  | cnt > 30 = nwMtr
  | otherwise = treeFinder mtr (y + 1) (x + 1) ( discoveryStr ((!!) mtr x) ((length $ (!!) mtr 0) - y -1) : nwMtr) (cnt + 1) 
  
newTree = reverse $ treeFinder working 0 0 [] 0