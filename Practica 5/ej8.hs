data Arbol a = Hoja a | Nodo a (Arbol a) (Arbol a) deriving Show

--Punto a
size :: Arbol a -> Int
size (Hoja _) = 1
size (Nodo _ l r) = 1 + (size l) + (size r)


--Punto b
--Demostrar: ∀t∈(Arbol a). ∃k ∈ N. size t = 2 k + 1
--Caso base: size (Hoja a) = 2*k + 1
size (Hoja a)
--Def size1
1
--Aritmetica
2 * 0 + 1
--Con k=0, queda demostrado el caso el base.

--Caso inductivo, asumo que P(l), P(r), pruebo para (Nodo a l r)
size (Nodo a l r)
--Def size2
1 + (size l) + (size r)
--HI
1 + (2*k1 + 1) + (2*k2 + 1)
--Aritmetica
3 + 2*(k1 + k2)
--Aritmetica
1 + 2*(k1 + k2 + 1)
--Defino k = (k1 + k2 + 1), tenemos asi size (Nodo a l r) = 2*k + 1, queda probado el caso inductivo. QED


--Punto c y d
mirror :: Arbol a -> Arbol a
mirror (Hoja a) = Hoja a
mirror (Nodo a l r) = Nodo a (mirror r) (mirror l)

--Probamos que mirror . mirror = id
--Caso base: mirror . mirror (Hoja a) = (Hoja a)
mirror . mirror (Hoja a)
--Def composicion
mirror (mirror (Hoja a))
--Def mirror1
mirror (Hoja a)
--Def mirror1
Hoja a
--Def id
id (Hoja a)
--Queda probado el caso base

--Caso inductivo, asumo que P(l), P(r), pruebo para (Nodo a l r)
mirror . mirror (Nodo a l r)
--Def composicion
mirror (mirror (Nodo a l r))
--Def mirror2
mirror (Nodo a (mirror r) (mirror l))
--Def mirror2
Nodo a (mirror (mirror l)) (mirror (mirror r))
--HI
Nodo a l r
--Def id
id (Nodo a l r)
--Queda probado el caso inductivo. QED

--Punto e
hojas :: Arbol a → Int
hojas (Hoja x) = 1
hojas (Nodo x t1 t2) = hojas t1 + hojas t2

altura :: Arbol a → Int
altura (Hoja x) = 1
altura (Nodo x t1 t2) = 1 + (altura t1 `max` altura t2)

--Definimos P(t) = hojas t < 2^(altura t), vemos que P(t) vale para todo arbol finito.
--Caso base, probamos para (Hoja a)
hojas (Hoja a)
--Def hojas1
1

--Viendo (altura (Hoja a)) tenemos
altura (Hoja a)
--Def altura1
1
--Tenemos asi 2^(altura (Hoja a)) = 2^1 = 2
--Finalmente 1 < 2^1 = 2
--Queda demostrado el caso base

--Caso inductivo, asumo que P(l), P(r), pruebo para (Nodo a l r)
hojas (Nodo a l r)
--Def hojas2
hojas l + hojas r
--HI
< 2^(altura l) + 2^(altura r)
--Sin perdida de generalidad asumo que (altura l) >= (altura r), con 2^n creciente, tenemos asi:
<= 2^(altura l) + 2^(altura l)
--Aritmetica
= 2^(1 + (altura l))

--Viendo el otro lado de la ecuacion
2^(altura (Nodo a l r))
--Def altura2
2^(1 + (altura l `max` altura r))
2^(1 + altura l)

--Tenemos asi que vale P(Nodo a l r), por lo que queda probado el caso inductivo. QED