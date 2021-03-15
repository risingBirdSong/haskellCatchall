import Data.List

-- Simple Array Sum

main = do
    n <- getLine
    ns <- map read . words <$> getLine
    print $ sum ns