zip' :: [a] -> [b] -> [(a, b)]
zip' [ ] ys = [ ]
zip' (x : xs) [ ] = [ ]
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

unzip' :: [(a, b)] -> ([a], [b])
unzip' [ ] = ([ ], [ ])
unzip' ((x, y) : ps) = (x : xs, y : ys)
    where (xs, ys) = unzip' ps


--unzip (uncurry zip)
--Caso base: []
((uncurry zip) . unzip) []
--<Def composicion>
uncurry zip' (unzip [])
--<Def unzip'1>
uncurry zip' ([], [])
--<Def uncurry, zip'1>
[]
--<Def id>
id []
--Queda probado el caso base.

--Caso inductivo, asumo que vale para [(xs, ys)] pruebo para (x, y):ps
((uncurry zip) . unzip) (x, y):ps
--<Def composicion>
uncurry zip' (unzip (x, y):ps)
--<Def unzip'2>
uncurry zip' ((x : xs, y : ys) where (xs, ys) = unzip' ps)
--<Def uncurry, zip'3>
(x, y) : zip' xs ys where (xs, ys) = unzip' ps
--<Def uncurry>
(x, y) : (uncurry zip (xs, ys) where (xs, ys) = unzip' ps)
--<Def where>
(x, y) : (uncurry zip (unzip' ps))
--<HI>
(x, y) : (id ps)
--<Def id>
(x, y):ps
--<Def id>
id ((x, y):ps)
--Queda probado el caso inductivo