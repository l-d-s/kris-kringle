module Main where

import System.Console.ANSI
import Permutations

main :: IO ()
main =
    do introduce
       nameList <- words <$> getLine
       pairs <- randomDerangement nameList
       mapM_ printNamePair pairs

introduce :: IO ()
introduce =
    do putStrLn "Enter participants' names,\n\
                \separated by spaces:"

noInputPrompt :: String -> IO ()
noInputPrompt string =
    do putStrLn string
       getChar
       clearScreen

printNamePair :: (String, String) -> IO ()
printNamePair (name1, name2) =
    mapM_ noInputPrompt
        [ "Please fetch " ++ name1 ++ "."
        , name1 ++ ", please press any key."
        , name1 ++ ", your Kris Kringle is " ++ name2 ++ ". Please press any key."
        ]
