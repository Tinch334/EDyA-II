--Algebraic specification
--Constructors
empty :: Queue A
push :: A -> Queue A -> Queue A

--Functions
isEmpty :: Queue A -> Bool
isEmpty empty = True
isEmpty (push x q) = False

top :: Queue A -> A
top (push x q) = x

pop :: Queue A -> Queue A
pop empty = empty
pop (push x q) = q


--Sequence specification, using: <x1, x2, ... xn>
empty = <>
push x <x1, x2, ... xn> = <x, x1, x2, ... xn>

top <x1, x2, ... xn> = x1
pop <x1, x2, ... xn> = <x2, ... xn>

empty <x1, x2, ... xn> = True -- if n = 0
empty <x1, x2, ... xn> = False --otherwise