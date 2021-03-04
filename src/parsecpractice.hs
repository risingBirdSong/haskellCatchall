import Text.Parsec (parse, (<|>), many, many1, between)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (noneOf, char)

reverseInParentheses :: String -> String
reverseInParentheses s = case parse ripP "" s of
    Right x -> x
    Left x -> error . show $ x

ripP :: Parser String
ripP = concat <$> many (normalP <|> reverseP)

normalP :: Parser String
normalP = many1 (noneOf "()")

reverseP :: Parser String
reverseP = reverse <$> between (char '(') (char ')') ripP

--  foo(bar(baz))blim
-- "foo(bar)baz(blim)"