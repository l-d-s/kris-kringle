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
    randomShuffle' l []
    where
        randomShuffle' [] acc = return acc
        randomShuffle' l acc =
            do k <- randomRIO (0, length l - 1)
               let (lead, x:xs) = splitAt k l in
                   randomShuffle' (lead ++ xs) (x:acc)

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
