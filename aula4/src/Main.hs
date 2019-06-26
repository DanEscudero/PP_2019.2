raiz2Grau :: (Ord a, Floating a) => a -> a -> a -> (a, a)
raiz2Grau a b c = 
  if customSignum delta == -1
  then error "Delta negativo" -- ruim, porque o erro so ocorre ao chamar o valor. Pode ser muito depois da funcao ser chamada (lazy)
  else (x1, x2)
    where
      x1 = (-b + sqDelta) / (2 * a)
      x2 = (-b - sqDelta) / (2 * a)
      sqDelta = sqrt delta
      delta = b ^ 2 - 4 * a * c

euclidiana :: Floating a => a -> a -> a
euclidiana x y = sqrt diffSq
    where diffSq = (x - y) ^ 2

{- Guard Expressions -}
customSignum :: (Ord a, Num a, Num b) => a -> b
customSignum n | n == 0    = 0
              | n > 0     = 1
              | otherwise = -1

{- Pattern Matching -}
mul :: (Eq a, Num a) => a -> a -> a
mul _ 0 = 0
mul 0 _ = 0
mul x 1 = x
mul 1 y = y
mul x y = x * y

fat 0 = 1
fat n = n * (n - 1)

-- and
(&*) :: Bool -> Bool -> Bool
True &* True = True
_    &* _    = False

-- or
(|*) :: Bool -> Bool -> Bool
False |* False = False
_     |* _     = True

-- xor
(^*) :: Bool -> Bool -> Bool
False ^* True = True
True ^* False = True
_    ^* _     = False

mediaPonderada :: (Eq a, Floating a) => a -> a -> (a -> a -> a)
mediaPonderada w1 w2 p1 p2 =
  if w1 + w2 /= 1
  then error "Pesos nao somam 1"
  else p1 * w1 + p2 * w2

converteNota :: (Ord a, Floating a) => a -> Char
converteNota nota
  | nota < 5 = 'F'
  | nota < 6 = 'D'
  | nota < 7 = 'C'
  | nota < 8 = 'B'
  | otherwise = 'A'

conceitoFinal :: Char -> Char -> Char
conceitoFinal 'F' _ = 'F'
conceitoFinal _ 'F' = 'F'

conceitoFinal 'D' 'A' = 'C'
conceitoFinal 'D' 'B' = 'C'
conceitoFinal 'D' _ = 'D'

conceitoFinal 'C' 'A' = 'B'
conceitoFinal 'C' _ = 'C'

conceitoFinal 'B' 'D' = 'C'
conceitoFinal 'B' _ = 'B'

conceitoFinal 'A' 'A' = 'A'
conceitoFinal 'A' 'B' = 'A'
conceitoFinal 'A' _ = 'B'

turmaA1Pratica = mediaPonderada 0.4 0.6
turmaA1Teorica = mediaPonderada 0.3 0.7

p1A1P = 3
p2A1P = 8
p1A1T = 7
p2A1T = 10

mediaP = turmaA1Pratica p1A1P p2A1P
mediaT = turmaA1Teorica p1A1T p2A1T

finalA1 = conceitoFinal (converteNota mediaP) (converteNota mediaT)

turmaA2Pratica = mediaPonderada 0.4 0.6
turmaA2Teorica = mediaPonderada 0.3 0.9

p1A2P = 3
p2A2P = 8
p1A2T = 7
p2A2T = 10

mediaA2P = turmaA2Pratica p1A2P p2A2P
mediaA2T = turmaA2Teorica p1A2T p2A2T

finalA2 = conceitoFinal (converteNota mediaA2P) (converteNota mediaA2T)