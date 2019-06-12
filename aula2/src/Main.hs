-- soma :: (Int, Int) -> Int
-- soma (x,y) = x + y

zeroAteN :: Int -> [Int]
zeroAteN n = [0..n]

bools :: [Bool]
bools = [True,False]

nums :: [[Int]]
nums = [[1],[2,2][3,3,3]]

soma :: Int -> Int -> Int -> Int
soma x y z = x + y + z

copia :: x -> (x, x)
copia x = (x, x)

f :: a -> a
f x = x

g :: Eq a => a -> (a, a) -> Bool
g x (y, z) = True

h :: Num a => Int -> a -> a
h x y = y