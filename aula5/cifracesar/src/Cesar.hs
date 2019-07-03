module Cesar where
import Data.Char

-- converte uma letra minuscula para inteiro
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- converte um inteiro para letra minuscula
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
  0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
  6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

shift :: Int -> Char -> Char
shift n c = int2let ((let2int c + n) `mod` 26)

encode :: Int -> String -> String
encode n = map (shift n)

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs

-- quantidade de letras minúsculas em uma String
lowers :: String -> Int
lowers cs = length $ filter (\c -> c >= 0 && c <= 25) $ map let2int cs

-- conta a ocorrência de um caracter em uma String
count :: Char -> String -> Int
count x cs = length $ filter ( == x) cs

-- dado um n e m, calcule 100*n/m
percent :: Int -> Int -> Float
percent n m = 100.0 * fromIntegral n / fromIntegral m

-- calcule a porcentagem de cada letra minuscula
-- do alfabeto em uma String
-- a porcentagem é a contagem de ocorrência pelo total
-- de letras minúsculas
freqs :: String -> [Float]
freqs cs = map (\c -> percent (count c cs) (length cs)) cs

-- Calcule a medida de Chi-Quadrado de duas
-- tabelas de frequência:
-- Soma (Observado - Esperado)^2 / Esperado
chisqr :: [Float] -> [Float] -> Float
chisqr obs expec = sum $ zipWith error obs expec
  where error obs expec = (obs - expec) ^ 2 / expec

-- rotaciona uma tabela em n posições
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- retorna a lista de posições que contém um
-- elemento x
positions :: Eq a => a -> [a] -> [Int]







