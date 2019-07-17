import Test.QuickCheck

import Prelude hiding (and, concat, replicate, (!!), elem, min, minimum)
import Data.List hiding (and, concat, replicate, (!!), elem, min, minimum)

and :: [Bool] -> Bool
and [] = True
and [True] = True
and [False] = False
and (x:xs) = x && (and xs)

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]
-- concat [] = []
-- concat ([]:xs) = concat xs
-- concat ((x:xs):ys) = x : concat (xs:ys)

replicate :: Int -> a -> [a]
replicate 0 x = []
replicate 1 x = [x]
replicate n x = x : replicate (n-1) x

(!!) :: Int -> [a] -> a
0 !! (x:xs) = x
n !! (x:xs) = (n - 1) !! xs

elem :: Eq a => a -> [a] -> Bool
elem n [] = False
elem n (x:xs) = n == x || elem n xs

min :: Ord a => a -> a -> a
min a b
    | a < b = a
    | otherwise = b

minimum :: Ord a => [a] -> a
minimum (x:xs) = foldl min x xs

selectionSort :: Ord a => [a] -> [a]
selectionSort xs = selectionSort' xs []
    where
        selectionSort' [] l = l
        selectionSort' (x:xs) l = selectionSort' xs (minimum (x:xs) : l)

merge :: Ord a => [a] -> [a] -> [a]
merge m@(x:xs) n@(y:ys)
    | x < y = x : merge xs n
    | otherwise = y : merge m ys

mergeSort :: Ord a => [a] -> [a]
mergeSort xs = merge (mergeSort metadeEsq) (mergeSort metadeDir)
    where
        (metadeEsq, metadeDir) = splitAt half xs
        half = length xs `div` 2

prop_idempotencia :: Ord a => [a] -> Bool
prop_idempotencia xs = mergeSort (mergeSort xs) == mergeSort xs

prop_length :: Ord a => [a] -> Bool
prop_length xs = length (mergeSort xs) == length xs

prop_minimum :: Ord a => [a] -> Property
prop_minimum xs = not (null xs) ==> minimum xs == head (mergeSort xs)

prop_sorted :: Ord a => [a] -> Bool
prop_sorted xs = mergeSort xs == sort xs