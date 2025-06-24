data Tree a = E | Leaf a | Join (Tree a) (Tree a) deriving Show

(|||) :: a -> b -> (a, b)
x ||| y = (x, y)

dropT :: Tree a -> Tree a
dropT (Leaf _) = E
dropT (Join E r) = Join E (dropT r)
dropT (Join l r) = let l' = dropT l in 
    case l' of 
        E -> r
        (Join E E) -> r
        otherwise -> Join l' r

sufijos t = fst (sufijosI t E) where
    sufijosI :: Tree Int -> Tree Int -> (Tree (Tree Int), Tree Int)
    sufijosI E s = (E, s)
    sufijosI l@(Leaf a) s = (Leaf s, Join l s)
    sufijosI (Join l r) s =
        let
            (sR, tR) = sufijosI r s
            (sL, tL) = sufijosI l tR
        in (Join sL sR, tL)

zipTree :: Tree a -> Tree b -> Tree (a, b)
zipTree E _ = E
zipTree _ E = E
zipTree (Leaf a) (Leaf b) = Leaf (a, b)
zipTree (Join l1 r1) (Join l2 r2) = Join (zipTree l1 l2) (zipTree r1 r2)

conSufijos :: Tree Int -> Tree (Int, Tree Int)
conSufijos t = zipTree t (sufijos t)


reduceT :: (a -> a -> a) -> a -> Tree a -> a
reduceT f e E = e
reduceT f e (Leaf x) = x
reduceT f e (Join l r) = let (l', r') = (reduceT f e l) ||| (reduceT f e r) in f l' r'

maxT t = reduceT max 0 t

mapreduce :: (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
mapreduce f g e = mr where
    mr E = e
    mr (Leaf a) = f a
    mr (Join l r) = let (l', r') = mr l ||| mr r in g l' r'

maxAll :: Tree (Tree Int) -> Int
maxAll t = mapreduce maxT max 0 t


--Stock calculation
mapTree :: (a -> b) -> b -> Tree a -> Tree b
mapTree _ d E = Leaf d
mapTree f _ (Leaf x) = Leaf (f x)
mapTree f d (Join l E) = Join (mapTree f d l) E
mapTree f d (Join E r) = Join E (mapTree f d r)
mapTree f d (Join l r) = let (l', r') = mapTree f d l ||| mapTree f d r in Join l' r'

bestStock :: Tree Int -> Int
bestStock t = maxAll (mapTree (\(x, tree) -> (mapTree (\a -> a - x) 0 tree)) E (conSufijos t))


tree :: Tree Int
tree = Join (Join (Leaf 10) (Leaf 15)) (Leaf 20)

bigTree :: Tree Int
bigTree = Join
            (Join
                (Join
                    (Leaf 1)
                    (Join (Leaf 2) (Leaf 3))
                )
                (Join
                    (Leaf 4)
                    (Join E (Leaf 5))
                )
            )
            (Join
                (Join
                    (Leaf 16)
                    (Leaf 7)
                )
                (Join
                    (Join (Leaf 8) E)
                    (Leaf 9)
                )
            )