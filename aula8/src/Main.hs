type Coord = (Int, Int)
type Trans = Coord -> Coord

paraCima :: Trans
paraCima (x,y) = (x,y)

data Dir = Norte | Sul | Leste | Oeste
data DiasDaSemana = Dom | Seg | Ter | Qua | Qui | Sex | Sab

para :: Dir -> Trans
para Sul (x,y) = (x,y-1)
para Norte (x,y) = (x,y+1)
para Leste (x,y) = (x+1,y)
para Oeste (x,y) = (x-1,y)

caminhar :: [Dir] -> Trans
caminhar ds coord = foldl (flip para) coord ds

-- caminhar [] coord = coord
-- caminhar (d:ds) coord = caminhar ds (para d coord)

data Ponto = Ponto Double Double deriving (Show)

dist :: Ponto -> Ponto -> Double
dist (Ponto x y) (Ponto x' y') = sqrt $ (x - x') ^ 2 + (y - y') ^ 2

data Forma = Circle Ponto Double | Rect Ponto Double Double deriving (Show)

area :: Forma -> Double
area (Rect p w h) = w * h
area (Circle p r) = 3.14 * r ^ 2

-- data Maybe a = Nothing | Just a

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

safeDiv :: Int -> Int -> Maybe Int
safeDiv a 0 = Nothing
safeDiv a b = Just (a `div` b)

divComErro :: Int -> Int -> Int
divComErro m n = case safeDiv m n of
                  Nothing -> error "divisao por 0"
                  Just x -> x

-- data Either a b = Left a | Right b

safeDiv' :: Int -> Int -> Either String Int
safeDiv' a 0 = Left "divisao por 0"
safeDiv' a b = Right (a `div` b)

data Fuzzy = Verdadeiro | Pertinencia Double | Falso deriving (Show)

fuzzifica x
  | x <= 0 = Falso
  | x >= 1 = Verdadeiro
  | otherwise = Pertinencia x

data Tree a = Leaf a | Node (Tree a) a (Tree a)

contem :: Eq a => Tree a -> a -> Bool
contem (Leaf y) x = x == y
contem (Node l y r) x = x == y || contem r x || contem l x

contem' (Leaf y) x = x == y
contem' (Node l y r) x
  | x == y = True
  | x < y = contem' l x
  | x > y = contem' r x

data Jogo = Pedra | Papel | Tesoura deriving (Ord, Eq)
ganhaDe :: (Jogo, Jogo) -> Bool
ganhaDe (Pedra, Tesoura) = True
ganhaDe (Tesoura, Pedra) = False
ganhaDe (x, y) = x > y

perdeDe :: (Jogo, Jogo) -> Bool
perdeDe = not . ganhaDe

ganhadores :: [(Jogo, Jogo)] -> [Bool]
ganhadores = map ganhaDe