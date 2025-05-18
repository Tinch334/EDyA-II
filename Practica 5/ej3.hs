vacio :: Conjunto A

insertar :: A → Conjunto A → Conjunto A
insertar x (insertar x c) = insertar x c
insertar x (insertar y c) = insertar y (insertar x c)

borrar :: A → Conjunto A → Conjunto A
borrar e vacio = vacio
--If the element was found we can return, an element can only appear once in a set.
borrar e (insertar x c) = if (e == x) then c else insertar x (borrar e c)

esVacio :: Conjunto A → Bool
esVacio vacio = True
esVacio (insertar x c) = False

union :: Conjunto A → Conjunto A → Conjunto A
union c1 vacio = c1
union vacio c2 = c2
union c1 (insertar x c2) = insertar x (union c1 c2)

enConjunto :: A -> Conjunto A -> Bool
enConjunto e vacio = False
enConjunto e (insertar x' c') = (e == x') and (interseccionAux e c')

--Non ideal solution, has O(n*m).
interseccion :: Conjunto A → Conjunto A → Conjunto A
interseccion c1 vacio = vacio
interseccion vacio c2 = vacio
interseccion (insertar x c1) c2 = if (enConjunto x c2) == True then insertar x (interseccion c1 c2) else interseccion c1 c2

resta :: Conjunto A → Conjunto A → Conjunto A
resta c1 vacio = c1
resta vacio c2 = vacio
resta (insertar x c1) c2 = if (enConjunto x c2) then resta c1 c2 else insertar x (resta c1 c2)

--Ver choose