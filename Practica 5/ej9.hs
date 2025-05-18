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
alturaAGT (Node x xs)
--alturaAGT2
1 + maximum' (map alturaAGT xs)
--HI
1 + maximum' (map (maxAGT . ponerProfs 1) xs)
--Def ponerProfs, concatenacion
1 + maximum' (map (maxAGT (ponerProfs 1)) xs)


(maxAGT . ponerProfs 1) (Node x xs)
--Def concatenacion
maxAGT (ponerProfs 1 (Node x xs))
--Def ponerProfs
maxAGT (Node 1 (map (ponerProfs 2) xs))
--Def maxAGT2
maximum' (1 : (map maxAGT (map (ponerProfs 2) xs)))
--Lema 1 = map (f . g) xs = map f (map g xs)
maximum' (1 : map (maxAGT (ponerProfs 2)) xs)
--Lema 2 = maximum' (1 : map (maxAGT (ponerProfs 2)) xs) = maximum' (map (maxAGT (ponerProfs 2)) xs)
maximum' (map (maxAGT (ponerProfs 2)) xs)
--Lema 3 = (maxAGT (ponerProfs 2)) xs = 1 + maxAGT (ponerProfs 1)) xs
maximum' (map (1 + maxAGT (ponerProfs 1)) xs)
--HI
maximum' (map (1 + alturaAGT) xs)



(maxAGT . ponerProfs 1) (Node x xs)
--Def concatenacion
maxAGT (ponerProfs 1 (Node x xs))
--Def ponerProfs
maxAGT (Node 1 (map (ponerProfs 2) xs))
--Def maxAGT2
maximum' (1 : (map maxAGT (map (ponerProfs 2) xs)))


--Tenemos asi LHS = RHS, por lo que queda probado el caso inductivo. Por lo tanto P(t) vale para todo GTree. QED

--Probamos los lemas, lema 2:
maximum' (1 : map (maxAGT (ponerProfs 2)) xs) = maximum' (map (maxAGT (ponerProfs 2)) xs)
--Caso base, con []:
maximum' (1 : map (maxAGT (ponerProfs 2)) []) = maximum' (map (maxAGT (ponerProfs 2)) [])
--Def map
maximum' (1:[]) = maximum ()