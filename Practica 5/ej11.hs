join :: [[A]] -> [A]
join [ ] = [ ]
join (xs : xss) = xs ++ join xss

singleton x = [x]


id = join . map singleton

Caso base, xs = []
Join . map singleton []
<def concat>
join (map singleton [])
<def map.1>
join []
<def join.1>
[]
<def id>
id []
Queda demostrado el caso base

P(xs): id xs = join . map singleton xs
Caso inductivo, asumo que P(xs), pruebo para x:xs
Join . map singleton x:xs
<def concat>
join (map singleton x:xs)
<def map>
join (singleton x):(map singleton xs)
<def singleton>
join [x]:(map singleton xs)
<def join.2>
[x]++(join (map singleton xs))
<HI>
[x] ++ id xs
<id>
[x] ++ xs
<def ++>
x:xs
<def id>
id (x:xs)
QED

join.join::[[[A]]]->[A]
join . map join::[[[A]]]->[A]

join . join = join . map join

Caso base xs=[]:
Lado izquierdo:
Join . join []
<def concat>
join (join [])
<def join.1>
join []
<def join.1>
[]

Lado derecho:
join . map join []
<def concat>
join (map join [])
<def map.1>
join []
<def join.1>
[]

Como LHS = RHS el caso base queda probado



Caso inductivo
P(t) = join . join xsss = join . map join xsss
Asumo que P(xsss) / xsss::[[[A]]], pruebo para xss:xsss

(join . join) xss:xsss
<def .>
join(join xss:xsss)
<def join>
join(xss ++ join(xsss))



Lado derecho:
(join . (map join)) xss:xsss
<def .>
join (map join xss:xsss)
<def map.2>
join ((join xss):(map join xsss))
<def join2>
(join xss)++(join (map join xsss))
<HI>
(join xss)++(join (join xsss))
Lema 1: (join xss)++(join (join xsss)) = join(xss ++ join(xsss))
join(xss ++ join(xsss))
QED


Lema 1: (join xss)++(join (join xsss)) = join(xss ++ join(xsss))
Sea xss::[[A]], fija inducimos sobre xsss::[[[A]]]

Caso base:
(join xss)++(join (join []))
<def join.1>
(join xss)++(join [])
<def join.1>
(join xss)++ []
<def ++>
join xss
<def ++>
Join (xss ++ [])
<def join.1>
join (xss ++ (join []))
<Hipotesis>
join (xss ++ (join xsss))
Queda probado el caso base

Sea P(xsss): (join xss)++(join (join xsss)) = join(xss ++ join(xsss))

Asumo que P(xsss), pruebo para yss:xsss

Lado izquierdo:
(join xss)++(join (join yss:xsss))
<def join.2>
(join xss)++(join (yss ++ join xsss))
<HI, pues yss::[[A]]>
(join xss)++(join yss)++(join (join xsss))


Lado derecho:
 join(xss ++ join(yss:xsss))
<def join.2>
 join(xss ++ yss ++ (join xsss))
Lema 2: join(xss ++ yss) = join xss ++ join yss
(join xss)++(join yss)++(join (join xsss))

Como LHS = RHS queda probado el caso inductivo QED


Lema 2: join(xss ++ yss) = join xss ++ join yss

Caso base

Xss = []

join([] ++ yss)
<def ++>
join yss
<def ++>
[] ++ Join yss 
<def join1>
join [] ++ Join yss
<hipotesis>
join xss ++ join yss

Sea P(xss): (join xss)++join(yss) = join (xss ++ yss)

Pruebo para xs:xss

join(xs:xss) ++ join(yss)
<def join2>
xs ++ (join xss) ++ (join yss)
<HI>
xs ++ join (xss ++ yss)
<def join2>
join(xs:(xss ++ yss))
<def asoc ++>
join (xs:xss ++ yss)
Me aburri