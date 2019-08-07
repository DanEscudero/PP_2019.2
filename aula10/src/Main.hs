module Main where

import Control.Monad
import Control.Applicative
import Data.List

perms :: (Bounded a, Enum a) => [[a]]
perms = permutations [minBound..maxBound]

data Nacao = Suecia | Dinamarca | Noruega | Alemanha | Inglaterra
  deriving (Eq, Show, Bounded, Enum)

nacao :: [Nacao]
nacao = head perms

nacoes :: [[Nacao]]
nacoes = perms

data Pet = Cavalo | Zebra | Gato | Passaro | Cachorro
  deriving (Eq, Show, Bounded, Enum)

pet :: [Pet]
pet = head pets

pets :: [[Pet]]
pets = perms

data Cor = Verde | Azul | Vermelho | Branco | Amarelo
  deriving (Eq, Show, Bounded, Enum)

cor :: [Cor]
cor = head perms

cores :: [[Cor]]
cores = perms

data Bebida = Cerveja | Leite | Cha | Cafe | Agua
  deriving (Eq, Show, Bounded, Enum)

bebida :: [Bebida]
bebida = head perms

bebidas :: [[Bebida]]
bebidas = perms

data Cigarro = PallMall | BlueMaster | Dunhill | Prince | Blend
  deriving (Eq, Show, Bounded, Enum)

cigarro :: [Cigarro]
cigarro = head perms

cigarros :: [[Cigarro]]
cigarros = perms

solucoes = [(c, p) | c <- pet, p <- cigarro]

mesmaCasa :: (Eq a, Eq b) => a -> [a] -> b -> [b] -> Bool
mesmaCasa x' [] y' ys = False
mesmaCasa x' xs y' [] = False
mesmaCasa x' (x:xs) y' (y:ys) = x == x' && y == y' || mesmaCasa x' xs y' ys

resultador = do
  x1 <- lista1
  x2 <- lista2
  guard $ predicado  x1 x2
  return (x1, x2)

main :: IO ()
main = do print solucoes
