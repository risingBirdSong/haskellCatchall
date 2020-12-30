printTable table = 
  putStrLn $ unlines $ map (unwords . map (pad . show)) table
  where
  largest = maximum $ map (length . show) (concat table)
  pad str = replicate (max (largest - length str) 0) ' ' ++ str

printTableB table = 
  putStrLn $ unlines $ map (unwords . map (pad . show)) table
  where
  largest = maximum $ map (length . show) (concat table)
  pad str = replicate (max (largest - length str) 0) ' ' ++ str