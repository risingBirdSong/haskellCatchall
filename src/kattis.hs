import Data.List
import Data.List.Split
greetings xs = concatMap (\grp@(x:xs) -> if x == 'e' then grp ++ grp else grp) 
              $ split (condense $ oneOf "e") xs

main = interact (unlines . map greetings . lines)
