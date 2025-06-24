data Tree a = Leaf a | Node a (Tree a) (Tree a)

flatten (Leaf x) = [x]
flatten (Node x lt rt) = flatten lt ++ [x] ++ flatten rt

mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node x lt rt) = Node (f x) (mapTree f lt) (mapTree f rt)



map f . flatten = flatten . mapTree f

Caso base:
Lado izquierdo
map f . flatten (Leaf a)
<Def composicion>
map f (flatten  (Leaf a))
<Def flatten 1>
map f [a] z
<def map 1>
[f a]

Lado derecho:
flatten . mapTree f (Leaf a)
<def composicion>
flatten (mapTree f  (Leaf a))
<def. mapTree1>
flatten  (Leaf (f a))
<def flatten1>
[f a]

Queda probado el caso base

Caso inductivo:
P(t): (map f . flatten) t = (flatten . mapTree f) t
Sea (Node a l r) asumo que vale P(l) y P(r)

Lado izquierdo:
(map f . flatten) (Node a l r)
<def. composicion>
(map f) (flatten (Node a l r))
<def. flatten.2>
(map f) (flatten l ++ [a] ++ flatten r)
Lema 1: map f (xs++ys) = map f (xs) ++ map f (ys)
map f (flatten l) ++ map f [a] ++ map f (flatten r)
<HI, def map.1>
flatten (mapTree f l) ++ [f a] ++ flatten (mapTree f r)

Lado derecho:
(flatten . mapTree f) (Node a l r)
<def. composicion>
flatten (mapTree f (Node a l r))
<def. mapTree.2> 
flatten (Node (f x) (mapTree f l) (mapTree f r))
<def flatten.2>
flatten (mapTree f l) ++ [f a] ++ flatten (mapTree f r)

Queda probado el caso inductivo

Lema 1: map f (xs++ys) = map f (xs) ++ map f (ys)
Sea xs :: A, tenemos ys :: A una lista cualquiera
Caso base, probamos para xs = []
map f (xs++ys)
<Hipotesis>
Map f ([]++ys)
<def ++ . 1>
map f ys
<def ++ . 1>
[] ++ map f ys
<def map.1>
map f [] ++ map f ys
<Hipótesis>
map f xs ++ map f ys

Caso inductivo, probamos para x:xs
P(t) = map f (xs++ys) = map f xs ++ map f ys

Map f (x:xs ++ ys)
<def :>
Map f ([x]++ xs ++ ys)
<Def :>
Map f (x:(xs ++ ys))
<def map.2>
[f x] ++ map f (xs ++ ys))
<HI>
([f x] ++ map f xs) ++ map f ys
<def :>
((f x):map f xs) ++ map f ys
<def map2>
Map f x:xs ++ map f ys
QED
