module Main where

-- A basic kris kringle program.

import System.Random
import System.Console.ANSI


main = 
    do introduce
       nameList <- words <$> getLine
       pairs <- randomCyclePairs nameList
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
    mapM_ noInputPrompt $ 
        [ "Please fetch " ++ name1 ++ "."
        , name1 ++ ", please press any key."
        , name1 ++ ", your Kris Kringle is " ++ name2 ++ "."
        ]


-- Algorithm 1: Random cycle --                                     

-- Fisher-Yates shuffle
-- from http://en.literateprograms.org/Fisher-Yates_randomShuffle_%28Haskell%29#chunk%20def:randomShuffle%27

randomShuffle :: [a] -> IO [a]
randomShuffle l = 
    randomShuffle' l []
    where
        randomShuffle' [] acc = return acc
        randomShuffle' l acc =
            do k <- randomRIO (0, length l - 1)
               let (lead, x:xs) = splitAt k l in
                   randomShuffle' (lead ++ xs) (x:acc)

pairCycle :: [a] -> [(a,a)]
pairCycle []  = []
pairCycle [x] = []
pairCycle xs  = (last xs, head xs) : pairCycle' xs
    where
        pairCycle' [x]        = []
        pairCycle' (x1:x2:xs) = (x1, x2) : pairCycle' (x2 : xs)      
     
randomCyclePairs nameList = pairCycle <$> randomShuffle nameList

-- Check out algorithm described here: http://okmij.org/ftp/Haskell/perfect-shuffle.txt

-- Algorithm 2: Random derangement --

-- From Martínez, C., Panholzer, A., and Prodinger, H. (2008). Generating Random Derangements. In 2008 Proceedings of the Fifth Workshop on Analytic Algorithmics and Combinatorics (ANALCO), (Society for Industrial and Applied Mathematics), pp. 234–240.

-- Add option to email using smtp-mail / Network.Mail.SMTP. This would also
-- require formatting strings of the form " "First Last" <user@host.blah> ".

-- Allow input from the commandline (by words) or stdin using Turtle.
