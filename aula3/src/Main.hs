verdadeiro x y = x
falso x y = y
if_ b x y = b x y

not x = if_ x falso verdadeiro
and x1 x2 = x1 x2 falso
or x1 x2 = if_ x1 verdadeiro x2

pair x y b = if_ b x y
fst p = p verdadeiro -- p eh pair, verdadeiro eh o b do pair
snd p = p falso -- p eh pair, falso eh o b do pair