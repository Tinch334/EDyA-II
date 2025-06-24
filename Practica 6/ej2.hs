data Tree a = E | Leaf a | Join (Tree a) (Tree a)

(|||) :: a -> b -> (a, b)
x ||| y = (x, y)

mapreduce :: (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
mapreduce f g e = mr where
    mr E = e
    mr (Leaf a) = f a
    mr (Join l r ) = let (l', r') = mr l ||| mr r
        in g l' r'

mcss :: (Num a, Ord a) => Tree a -> a
mcss t = let (a, _, _, _) = mapreduce (\x -> (max x 0, max x 0, max x 0, x)) getSub (0, 0, 0, 0) t in
    a

getSub :: (Num a, Ord a) => (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
getSub (a1, a2, a3, a4) (b1, b2, b3, b4) = (maximum [a1, b1, a4 + b3], max a2 (a1 + b2), max b3 (b3 + a3), a4 + b4)