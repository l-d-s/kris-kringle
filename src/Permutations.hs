module Permutations
( randomPermutation
, randomCycle
, randomDerangement
) where

import System.Random

-- Ideally would use Arrays here (w/ Nat-indexed type?).
setElem :: Int -> a -> [a] -> [a]
setElem i x' xs =
    let (before, _ : after) = splitAt i xs
    in
        before ++ x' : after

swapElems :: Int -> Int -> [Int] -> [Int]
swapElems i j xs =
    setElem i (xs !! j) . setElem j (xs !! i) $ xs

-- Random permutation --

-- Fisher-Yates shuffle
-- from http://en.literateprograms.org/Fisher-Yates_shuffle_%28Haskell%29#chunk%20def:shuffle%27

randomShuffle :: [a] -> IO [a]
randomShuffle l =
    drawInto l []
    where
        drawInto [] drawnAlready = return drawnAlready
        drawInto l drawnAlready = do
                k <- randomRIO (0, length l - 1)
                let (before, x : after) = splitAt k l in
                    drawInto (before ++ after) (x : drawnAlready)

randomPermutation :: [a] -> IO [(a, a)]
randomPermutation xs = zip xs <$> randomShuffle xs

-- Random cycle --

-- Could also use "Sattolo's algorithm".
-- Is this a uniform projection?

pairCycle :: [a] -> [(a,a)]
pairCycle []  = []
pairCycle [x] = []
pairCycle xs  = (last xs, head xs) : pairCycle' xs
    where
        pairCycle' [x]        = []
        pairCycle' (x1:x2:xs) = (x1, x2) : pairCycle' (x2 : xs)

randomCycle :: [a] -> IO [(a, a)]
randomCycle xs = pairCycle . map snd <$> randomPermutation xs

-- Random derangement --

nDerangements :: Int -> Int
nDerangements n
    | n == 0    = 1
    | n == 1    = 0
    | otherwise =
        (n - 1) * (nDerangements (n - 1) + nDerangements (n - 2))

-- Ideally would avoid time-unbounded step, but it works.
repUntilM :: Monad m => (a -> Bool) -> m a -> m a
repUntilM pred action =
        do  x <- action
            if pred x
                then return x
                else repUntilM pred action

randomUniform :: IO Float
randomUniform = randomRIO ((0,1) :: (Float, Float))

-- Algorithm from Martínez et. al. 2008

rejectionProb :: Fractional a => Int -> a
rejectionProb u =
    fromIntegral (u - 1) * fromIntegral (nDerangements (u - 2)) /
        fromIntegral (nDerangements u)

randomDerangementN :: Int -> IO [Int]
randomDerangementN n =
    let
        i = n - 1 -- 0-based indexing
        u = n
        marks = replicate n False
        in
            randomDerangement' i u marks [1..n]

randomDerangement' :: Int -> Int -> [Bool] -> [Int] -> IO [Int]
randomDerangement' i u marks xs =
    if u >= 2
        then
            if not (marks !! i)
                then
                    do j <- repUntilM (\j -> not (marks !! j)) (randomRIO (0, i - 1))
                       let xs' = swapElems i j xs
                       p <- randomUniform
                       let p' = rejectionProb u
                       if p < p'
                           then
                               do let marks' = setElem j True marks
                                  let u' = u - 2
                                  randomDerangement' (i - 1) u' marks' xs'
                           else
                               do let u' = u - 1
                                  randomDerangement' (i - 1) u' marks xs'
            else
                randomDerangement' (i - 1) u marks xs
        else
            return xs

randomDerangement :: [a] -> IO [(a, a)]
randomDerangement xs =
    do
        let n = length xs
        derangedIndices <- randomDerangementN n
        return $ zip xs [xs !! (i - 1) | i <- derangedIndices]
