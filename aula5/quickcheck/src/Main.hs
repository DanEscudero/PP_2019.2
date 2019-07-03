import Data.List

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ rhs
                where
                  lhs = filter (<x) xs
                  rhs = filter (>=x) xs

prop_idempotencia :: Ord a => [a] -> Bool
prop_idempotencia xs = qsort (qsort xs) == qsort xs

prop_length :: Ord a => [a] -> Bool
prop_length xs = length (qsort xs) == length xs

prop_minimum :: Ord a => [a] -> Property
prop_minimum xs = not (null xs)
                  ==> minimum xs == head (qsort xs)

prop_sorted :: Ord a => [a] -> Bool
prop_sorted xs = qsort xs == sort xs

-----------------------

par :: Integral a => a -> Bool
par x = x `mod` 2 == 0

impar :: Integral a => a -> Bool
impar x = x `mod` 2 == 1

prop_alternaParImpar :: Integral a => a -> Bool
prop_alternaParImpar x = par x /= par (x+1)

prop_parOuImpar :: Integral a => a -> Property
prop_parOuImpar x = par x ==> not (impar x)

-----------------------

fatorial :: Integral a => a -> a
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

prop_fatorial1 :: Integral a => NonNegative a -> Bool
prop_fatorial1 (NonNegative n) = fatorial n * (n + 1) == fatorial (n + 1)

-----------------------

collatz :: Integral a => a -> a
collatz 1 = 1
collatz n 
  | par n = collatz (n `div` 2)
  | otherwise = collatz (3 * n + 1)

prop_collatz :: Integral a => Positive a -> Bool
prop_collatz (Positive n) = collatz n == 1