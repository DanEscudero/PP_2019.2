listAnd :: [Bool] -> Bool
listAnd [True] = True
listAnd [False] = False
listAnd (x:xs) = x && (listAnd xs)

customConcat :: [[a]] -> [a]
customConcat [] = []
customConcat ([]:xs) = customConcat xs
customConcat ((x:xs):ys) = x : customConcat (xs:ys)

customReplicate :: Int -> a -> [a]
customReplicate 0 x = []
customReplicate 1 x = [x]
customReplicate n x = x : customReplicate (n-1) x