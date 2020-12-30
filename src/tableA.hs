module Tables where

longestElem :: (Show a, Foldable t) => t [a] -> Int
longestElem table= 
    maximum $ (length . show) <$> concat table

findPaddingRow :: (Functor f, Show a1, Show a2, Foldable t) => f a1 -> t [a2] -> f Int
findPaddingRow row table =
    (\x -> longest - (length $ show x)) <$> row
    where longest = longestElem table

findPaddingTable :: (Functor f, Show a1, Foldable f) => f [a1] -> f [Int]
findPaddingTable table =
    (`findPaddingRow` table) <$> table

convertToWhitespaceNumber :: Int -> [Char]
convertToWhitespaceNumber number =
    replicate (number + 1) ' '

convertToWhitespaceRow :: Functor f => f Int -> f [Char]
convertToWhitespaceRow row =
    convertToWhitespaceNumber <$> row

convertToWhitespaceTable :: (Functor f1, Functor f2) => f1 (f2 Int) -> f1 (f2 [Char])
convertToWhitespaceTable table =
    convertToWhitespaceRow <$> table

showRow :: (Functor f, Show a) => f a -> f String
showRow row =
    show <$> row

showTable :: (Functor f1, Functor f2, Show a) => f1 (f2 a) -> f1 (f2 String)
showTable table =
    showRow <$> table

zipRows :: [[Char]] -> [[Char]] -> [[Char]]
zipRows rowOne rowTwo =
    (zipWith (++) rowTwo rowOne) ++ ["\n\n"]

padTable :: Show a => [[a]] -> [[[Char]]]
padTable table =
    [["\n"]] ++ zipWith (zipRows) (convertToWhitespaceTable $ findPaddingTable table) (showTable table) 

printRow :: Foldable t => t String -> IO ()
printRow row =
    mapM_ putStr row

printTable :: Show a => [[a]] -> IO ()
printTable table =
    mapM_ printRow (padTable table)