module Main where

dobra :: Num a => a -> a
dobra x = x + x

quadriplica :: Num a => a -> a
quadriplica x = dobra (dobra x)

fatorial :: Integral a => a -> a
fatorial n = product [1..n]

f :: Num a => a -> a
f x = a * x + b
  where
    a = 1
    b = 3

triplo :: Num a => a -> a
triplo x = 3 * x

--denom :: Fractional a => a -> a -> a
denom peso nota = peso * recip (maximum [0.1, nota])

-- media :: Num a => a -> a -> a -> a
media atividade prova projeto = 10 / sum [denom 4 atividade, denom 3 prova, denom 3 projeto]

main :: IO ()
main = do
  print(media 8 5 8)