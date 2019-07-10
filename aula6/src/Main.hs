module Main where

import Data.List

votos :: [String]
votos = ["Vermelho", "Azul", "Verde", "Azul", "Azul", "Vermelho"]

votosRanks :: [[String]]
votosRanks = [["Vermelho", "Verde"], ["Azul"], ["Verde", "Vermelho", "Azul"], ["Azul", "Verde", "Vermelho"], ["Verde"]]

-- conta quantos votos o candidato x recebeu
conta :: Eq a => a -> [a] -> Int
conta x xs = length $ filter ( == x) xs

-- retorna a lista de elementos unicos
unicos :: Eq a => [a] -> [a]
unicos = nub

-- retorna uma lista de pares ordenados (votos, candidato) com o total
-- de votos obtido por cada candidato use a função sort para ordenar
-- do menos para o mais votado
resultado :: Ord a => [a] -> [(Int, a)]
resultado xs = sort $ [(conta x xs, x) | x <- unicos xs]

-- retorna o vencedor da eleição
vencedor :: Ord a => [a] -> a
vencedor xs = snd $ head $ resultado xs

-- elimina as listas vazias de uma lista de listas
rmvazio :: Eq a => [[a]] -> [[a]]
rmvazio = filter (/= [])

-- elimina um candidato da lista de votos
elimina :: Eq a => a -> [[a]] -> [[a]]
elimina x xss = [filter (/= x) xs | xs <- xss]

-- retorna uma lista dos candidatos existentes, do menos para o mais votado
rank :: Ord a => [[a]] -> [a]
rank xss = map snd (reverse $ resultado $ concat xss)

-- retorna o vencedor executando o processo descrito acima
vencedor' :: Ord a => [[a]] -> a
vencedor' xss
  | (length $ unicos $ concat xss) == 1 = head $ concat xss
  | otherwise = elimina (rank $ rmvazio xss) xss

main :: IO()
main = do
  print $ vencedor votos
  print $ vencedor' votosRanks