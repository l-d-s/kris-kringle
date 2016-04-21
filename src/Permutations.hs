module Permutations
( randomPermutation
, randomCycle
) where

import System.Random

-- Random permutation --

-- Fisher-Yates shuffle
-- from http://en.literateprograms.org/Fisher-Yates_shuffle_%28Haskell%29#chunk%20def:shuffle%27

randomShuffle :: [a] -> IO [a]
randomShuffle l =
    drawInto l []
    where
        drawInto [] drawnAlready = return drawnAlready
        drawInto l drawnAlready =
            do k <- randomRIO (0, length l - 1)
               let (before, x:after) = splitAt k l in
                   drawInto (before ++ after) (x:drawnAlready)

randomPermutation :: [a] -> IO [(a, a)]
randomPermutation xs = (zip xs) <$> (randomShuffle xs)

-- Random cycle --

-- Could also use "Sattolo's algorithm".

pairCycle :: [a] -> [(a,a)]
pairCycle []  = []
pairCycle [x] = []
pairCycle xs  = (last xs, head xs) : pairCycle' xs
    where
        pairCycle' [x]        = []
        pairCycle' (x1:x2:xs) = (x1, x2) : pairCycle' (x2 : xs)

randomCycle :: [a] -> IO [(a, a)]
randomCycle xs = pairCycle <$> (map snd) <$> randomPermutation xs
