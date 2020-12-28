import qualified Data.Map as M
import System.Process
import Control.Monad
import Data.Traversable
import System.Environment
import System.Exit
import Data.List
import System.IO
import Test.QuickCheck
import Debug.Trace

clear = system "cls"

type Morse = String

-- , ('3', "...--")

letterToMorse :: ( M.Map Char Morse)
letterToMorse = M.fromList [
    ('a', ".-")
    , ('b', "-...")
    , ('c', "-.-.")
    , ('d', "-..")
    , ('e', ".")
    , ('f', "..-.")
    , ('g', "--.")
    , ('h', "....")
    , ('i', "..")
    , ('j', ".---")
    , ('k', "-.-")
    , ('l', ".-..")
    , ('m', "--")
    , ('n', "-.")
    , ('o', "---")
    , ('p', ".--.")
    , ('q', "--.-")
    , ('r', ".-.")
    , ('s', "...")
    , ('t', "-")
    , ('u', "..-")
    , ('v', "...-")
    , ('w', ".--")
    , ('x', "-..-")
    , ('y', "-.--")
    , ('z', "--..")
    , ('1', ".----")
    , ('2', "..---")
    , ('3', "...--")
    , ('4', "....-")
    , ('5', ".....")
    , ('6', "-....")
    , ('7', "--...")
    , ('8', "---..")
    , ('9', "----.")
    , ('0', "-----")
    ]

morseToLetter :: M.Map Morse Char
morseToLetter =
  M.foldrWithKey (flip M.insert) M.empty
  letterToMorse

-- fromList [("-",'t'),("--",'m'),("---",'o'),("-----",'0'),("----.",'9'),("---..",'8'),("--.",'g'),("--.-",'q'),("--..",'z'),("--...",'7'),("-.",'n'),("-.-",'k'),("-.--",'y'),("-.-.",'c'),("-..",'d'),("-..-",'x'),("-...",'b'),("-....",'6'),(".",'e'),(".-",'a'),(".--",'w'),(".---",'j'),(".----",'1'),(".--.",'p'),(".-.",'r'),(".-..",'l'),("..",'i'),("..-",'u'),("..---",'2'),("..-.",'f'),("...",'s'),("...-",'v'),("...--",'3'),("....",'h'),("....-",'4'),(".....",'5')]

charToMorse :: Char -> Maybe Morse
charToMorse c =
  M.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse s =
  sequence $ fmap charToMorse s

morseToChar :: Morse -> Maybe Char
morseToChar m =
  M.lookup m morseToLetter


morseToString s = sequence $ fmap morseToChar s

convertToMorse :: IO ()
convertToMorse = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess
  line <- hGetLine stdin
  if line == "qqq" then do 
    putStrLn "quitting..."
    exitSuccess
  else putStr ""
  convertLine line
  where
  convertLine line = do
    let morse = stringToMorse line
    case morse of
      (Just str)
        -> putStrLn
         (intercalate " " str)
      Nothing
        -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess
  -- otherwise, proceed.
  line <- hGetLine stdin
  convertLine line
  where
    convertLine line = do
      let decoded :: Maybe String
          decoded =
            traverse morseToChar
                    (words line)
      case decoded of
          (Just s) -> putStrLn s
          Nothing -> do
            putStrLn $ "ERROR: " ++ line
            exitFailure

main :: IO ()
main = do
  putStrLn "please enter mode, either from or to..."
  mode <- getLine 
  case mode of
    "from" -> convertFromMorse
    "to" -> convertToMorse
    _ -> argError
  where argError = do
        putStrLn "Please specify the\
                  \ first argument\
                  \ as being 'from' or\
                  \ 'to' morse,\
                  \ such as: morse to"
        exitFailure

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen
  (\c -> trace (show c) ((charToMorse c)
  >>= morseToChar) == Just c)

testmain :: IO ()
testmain = quickCheck ( prop_thereAndBackAgain)


