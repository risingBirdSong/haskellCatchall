module Myfirstmod where
rvrswrds str = unwords $ reverse $ words str

main = putStrLn $ rvrswrds "Hi I am a test of the module, I reverse" 