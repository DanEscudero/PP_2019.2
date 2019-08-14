module KMeansPar where

import Data.List
import Data.Array
import Data.Function
import Control.DeepSeq
import Control.Parallel.Strategies

type Ponto = [Double]
type Cluster = [Double]
newtype ChunksOf a = ChunksOf [a]

-- divide elementos de xs por x
(./) :: (Floating a, Functor f) => f a -> a -> f a
xs ./ x = (/x) <$> xs

-- eleva elementos de xs por x
(.^) :: (Floating a, Functor f) => f a -> Int -> f a
xs .^ x = (^x) <$> xs

-- soma dois vetores elemento a elemento
(.+.) :: (Num a) => [a] -> [a] -> [a]
(.+.) = zipWith (+)

-- subtrai dois vetores elemento a elemento
(.-.) :: (Num a) => [a] -> [a] -> [a]
(.-.) = zipWith (-)

length' :: Num b => [a] -> b
length' xs = fromIntegral $ length xs

assign :: [Cluster] -> [Ponto] -> Array Int [Ponto]
assign cs ps = accumArray (flip (:)) [zeroCluster] (0, k-1) [(argmin (distancias p), p) | p <- ps]
    where
      distancias p = [euclid p c | c <- cs]
      euclid   p c = sum $ ((.-.) . (.^2)) p c
      argmin    xs = fst $ minimumBy (compare `on` snd) $ zip [0..] xs
      zeroCluster  = replicate (length $ head ps) 0
      k            = length cs

somaClusters :: Array Int [Ponto] -> [(Cluster, Double)]
