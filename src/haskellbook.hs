-- let vs where

plusTwowhere n = print $ f n 
    where f n = n + 2 

plusTwolet n = let f n = 2 + n  
                in print $ f n 


aaa = let x = 333; y = 666 in x + y -- 999
aab :: Integer
aab = let x = 222; y = 333 in x * y -- 73926

aac = x * y where x = 67; y = 91 -- 6097

aad = x + y where x = 213; y = 4234 -- 4447

myGreeting :: String
myGreeting = (++) "hello" " world!"

main :: IO ()
main = do
putStrLn myGreeting
putStrLn secondGreeting
  where secondGreeting = "hi again"


aae =  'c' : "hris"
aaf = 'P' : ""


crry = "Curry is awesome"
aag = take 1 $ drop 4 crry -- "y"

takerdroper t d s= take t $ drop d s 