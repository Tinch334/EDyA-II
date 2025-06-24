data AGTree a = Node a [AGTree a] deriving Show

ponerProfs :: Int -> AGTree a -> AGTree Int 
ponerProfs n (Node x xs) = Node n (map (ponerProfs (n + 1)) xs)

maximum' :: Ord a => [a] -> a
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

--Punto a
alturaAGT :: AGTree a -> Int
alturaAGT (Node _ []) = 1
alturaAGT (Node _ xs) = 1 + maximum' (map alturaAGT xs)

--Punto b
maxAGT :: Ord a => AGTree a -> a
maxAGT (Node n []) = n
maxAGT (Node n xs') = maximum' (n : (map maxAGT xs'))


--Punto c
--Definimos P(t): alturaAGT t = (maxAGT . ponerProfs 1) t, lo probamos
--Caso base, probamos para (Node a [])
alturaAGT (Node a [])
--Def alturaAGT1
1

--Vemos el otro lado de la ecuacion:
(maxAGT . ponerProfs 1) (Node a [])
--Def composicion
maxAGT (ponerProfs 1 (Node a []))
--Def ponerProfs1
maxAGT (Node 1 map (ponerProfs (1 + 1)) [])
--Def map
maxAGT (Node 1 [])
--Def maxAGT1
1

--Tenemos asi LHS = RHS, por lo que queda probado el caso base.

--Probamos el caso inductivo, como tenemos un arbol general usamos otra HI. Sea (Node x xs), asumimos que P vale para todo x∈xs, probamos para
--el nodo (Node x xs)
--Vemos el caso donde xs!=[]
alturaAGT (Node x xs)
-- <alturaAGT2>
1 + maximum' (map alturaAGT xs) 
-- <HI>
1 + maximum' (map (maxAGT . (ponerProfs 1)) xs)
-- Lema 2
1 + maximum' (1:(map (maxAGT . (ponerProfs 1)) xs))
-- map (f . g) xs = map f (map g xs)
1 + maximum' (1 : (map maxAGT (map (ponerProfs 1) xs)))
-- <Def maxAGT2>
1 + maxAGT (Node 1 (map (ponerProfs 1) xs))
-- <Def ponerProfs>
-- Lema 3: ponerProfs x (Node x xs) = ponerProfs x (Node y xs) pt y
1 + maxAGT (ponerProfs 0 (Node x xs))
-- Lema 1: 1 + maxAGT (ponerProfs n (Node a xs)) = maxAGT (ponerProfs n + 1 (Node a xs))
maxAGT (ponerProfs (0 + 1) (Node x xs))
-- <Aritmetica>
maxAGT (ponerProfs 1 (Node x xs))
-- <Def composicion>
(maxAGT . ponerProfs 1) (Node x xs)
--Queda probado el caso inductivo, QED.


{-
   Lema 1: 1 + maxAGT (ponerProfs n (Node a xs)) = maxAGT (ponerProfs n + 1 (Node a xs))
   P(xs) = 1 + maxAGT (ponerProfs n (Node a xs)) = maxAGT (ponerProfs n + 1 (Node a xs))
-}
--Caso base (Node a []):
maxAGT . (ponerProfs 1 + 1) (Node a [])
-- <Algebra y def .>
maxAGT (ponerProfs 2 (Node a []))
-- <Def ponerProfs, map1>
maxAGT (Node 2 [])
-- <Def maxAGT>
2
--LHS
-- <Aritmetica>
1 + 1
-- <maxAGT1>
1 + maxAGT (Node a [])
-- <Def ponerProfs, map1>
1 + (maxAGT (ponerProfs 1 (Node a [])))
-- <Def .>
1 + (maxAGT . (ponerProfs 1) (Node a []))
--RHS
--Queda probado el caso base.

--Caso inductivo, asumo que vale P(x) para x∈xs, pruebo para (Node a xs)
maxAGT . (ponerProfs n + 1) (Node a xs)
-- <Def .>
maxAGT (ponerProfs n + 1 (Node a xs))
-- <Def ponerProfs>
maxAGT (Node (n + 1) map (ponerProfs (n + 1 + 1)) xs)
-- <Aritmetica>
maxAGT (Node (n + 1) map (ponerProfs (n + 2)) xs)
-- <Def maxAGT>
maximum' (n + 1):(map (maxAGT) (map (ponerProfs (n + 2)) xs))
-- map (f . g) xs = map f (map g xs)
maximum' (n + 1):(map maxAGT . (ponerProfs (n + 1 + 1)) xs)
-- <HI>
maximum' (n + 1):(map (1 + maxAGT . (ponerProfs (n + 1))) xs)
-- map (f . g) xs = map f (map g xs)
maximum' (n + 1):(map (1 + maxAGT) (map (ponerProfs (n + 1)) xs))
-- <Def map>
maximum' (n + 1):(map (+ 1)) (map maxAGT (map (ponerProfs (n + 1)) xs)))
-- Lema 1': maximum' (n + 1):(map 1 (+) xs) = 1 + maximum' (n:xs)
1 + maximum n:(map maxAGT (map (ponerProfs (n + 1)) xs))
-- <Def maxAGT.2>
1 + maxAGT (Node n (map (ponerProfs (n + 1)) xs))
-- <Def ponerProfs>
1 + maxAGT (ponerProfs n (Node a xs))
--Queda asi probado el caso inductivo, QED.

{-
   Lema 2: maximum' (1 : xs) = maximum' xs / Existe x∈xs x > 1
   P(xs) = maximum' (1 : xs) = maximum' xs / Existe x∈xs x > 1
-}
--Caso base xs = [n] / n > 1
maximum' (1 : [n])
-- <Def maximum'2>
max 1 (maximum' [n])
-- <Def maximum'1>
max 1 n
-- <Def max y n > 1>
n
-- <Def maximum'1>
maximum' [n] = maximum' xs
--Queda probado el caso base.

--Caso inductivo, supongo que P(xs), pruebo para x:xs
maximum' (1 : (x:xs))
-- <Def maximum'2>
max 1 (maximum' (x:xs))
-- <Def maximum'2>
max 1 (max x (maximum' xs))
-- Sea n = maximum' xs, por HI se que n > 1
max 1 (max x n)
--Si x > n ==> x > 1, si n > x sabemos n > 1, en cualquier caso max 1 (max x n) > 1
--Queda probado el caso inductivo, QED.