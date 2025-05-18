--a
--Constructors
nil :: List A
cons :: A -> List A -> List A

--Functions
null :: List A -> Bool
null nil = True
null (cons x xs) = False

head :: List A -> A
head (cons x xs) = x

tail :: List A -> List A
tail nil = nil
tail (cons x xs) = xs

--b
--We use sequences: <x1, x2, ... xn>
nil = <>
cons x <x1, x2, ... xn> = <x, x1, x2, ... xn>
head <x1, x2, ... xn> = x1
tail <x1, x2, ... xn> = <x2, ... xn>

null <x1, x2, ... xn> = True --if n = 0
null <x1, x2, ... xn> = False --otherwise

--c
inL :: List A -> A -> Bool
inL nil e = False
inL (cons x xs) e = (x == e) and (inL xs e)

--d
removeL :: List A -> A -> List A
removeL nil e = nil
removeL (cons x xs) e = if (x == e) then removeL xs e else cons x (removeL xs e)