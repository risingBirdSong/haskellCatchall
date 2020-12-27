
type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String deriving (Eq, Show)
mkPerson :: Name
          -> Age
          -> Either PersonInvalid Person
mkPerson name age
      | name /= "" && age > 0 = Right $ Person name age
      | name == "" = Left NameEmpty
      | not (age > 0) = Left AgeTooLow
      | otherwise = Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do 
  putStrLn "hi what is the person's name?"
  rawname <- getLine 
  putStrLn "what is the person's age?"
  rawage <- getLine
  let age = (\x -> read x :: Integer) rawage 
  let person = mkPerson rawname age
  print person
  let outcome (Right ( Person a b)) = "success!"
      outcome (Left e) = "failure!" ++ show e 
      outcome _ = "hmm"
  print (outcome person)
  putStrLn "done"

doIt = do
  let f (Just n) = "Just"
      f Nothing = "Nothing"
  putStrLn . f $ Just 5

doItBroke = do
  let f (Just n) = "Just"
  let f Nothing = "Nothing"
  putStrLn . f $ Just 5