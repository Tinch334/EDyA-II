data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

(|||) :: a -> b -> (a, b)
x ||| y = (x, y)

size :: BTree a -> Int
size Empty = 0
size (Node n _ _ _) = n

nth :: BTree a -> Int -> a
nth Empty _  = error "Invalid index, empty tree"
nth (Node s l a r) index    | index == (size l) = a
                            | index < (size l) = nth l index
                            | otherwise = nth r (index - (size l) - 1)

cons :: a -> BTree a -> BTree a
cons elem Empty = Node 1 Empty elem Empty
cons elem (Node s l a r) = Node (s + 1) (cons elem l) a r


tabulate :: (Int -> a) -> Int -> BTree a
tabulate f n    | n <= 0 = Empty
                | otherwise =
                let
                    m = div n 2
                    (lTree, rTree)= tabulate f m ||| tabulate (\x -> f (m + 1 + x)) (n - m - 1)
                    newSize = (size lTree) + (size rTree) + 1
                in
                    Node newSize lTree (f m) rTree


myid :: Int -> Int
myid x = x

mapBT :: (a -> b) -> BTree a -> BTree b
mapBT _ Empty = Empty
mapBT f (Node s l a r) = let ((l', r'), a') = (mapBT f l ||| mapBT f r) ||| (f a) in Node s l' a' r'

takeBT :: BTree a -> Int -> BTree a
takeBT tree n = takeBTInner tree (n - 1) where
    takeBTInner :: BTree a -> Int -> BTree a
    takeBTInner (Node s l a r) n    | n < lSize = takeBT l n
                                    | n == lSize = l
                                    | n == lSize + 1 = Node (lSize + 1) l a Empty
                                    | otherwise = 
                                    let 
                                        r' = takeBT r (n - lSize - 1)
                                        newRSize = size r'
                                        newSize = 1 + lSize + newRSize
                                    in Node newSize l a r'
                                where lSize = size l

dropBT :: BTree a -> Int -> BTree a
dropBT tree n = dropLast tree ((size tree) - n + 1) where
    dropLast :: BTree a -> Int -> BTree a
    dropLast (Node s l a r) n   | n < rSize = dropLast r n
                                | n == rSize = r
                                | n == rSize + 1 = Node (rSize + 1) Empty a r
                                | otherwise = 
                                    let 
                                        l' = dropLast l (n - rSize - 1)
                                        newLSize = size l'
                                        newSize = 1 + rSize + newLSize
                                    in Node newSize l' a r
                                where rSize = size r


exampleTree :: BTree Int
exampleTree =
  Node 5
    ( Node 2
        (Node 1 Empty 1 Empty)   -- left child is a leaf holding 1 (subtree‐size = 1)
        2                         -- this node holds 2 (subtree‐size = 2)
        (Node 1 Empty 10 Empty)                     -- no right child
    )
    3                             -- root holds 3
    ( Node 2
        Empty                     -- no left child
        4                         -- this node holds 4 (subtree‐size = 2)
        (Node 1 Empty 5 Empty)    -- right child is a leaf holding 5 (subtree‐size = 1)
    )