data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

(|||) :: a -> b -> (a, b)
x ||| y = (x, y)

size :: BTree a -> Int
size Empty = 0
size (Node n _ _ _) = n

toList :: BTree a -> [a]
toList Empty = []
toList (Node _ l a r) = (toList l) ++ [a] ++ (toList r )

splitAtBT :: BTree a -> Int -> (BTree a, BTree a)
splitAtBT Empty _ = (Empty, Empty)
splitAtBT (Node _ l a r) n  | sizeL > n = let (ll, lr) = splitAtBT l n in (ll, makeNode lr a r)
                            | sizeL == n = (l, makeNode Empty a r)
                            | sizeL + 1 == n = (makeNode l a Empty, r)
                            | otherwise =
                                let
                                    (rl, rr) = splitAtBT r (n - sizeL - 1)
                                in (makeNode l a rl, rr)
                            where sizeL = size l

--Auxiliary function, builds node from two subtrees and a value.
makeNode :: BTree a -> a -> BTree a -> BTree a
makeNode l a r = Node (size l + size r + 1) l a r


rebalanceBT :: BTree a -> BTree a
rebalanceBT t = makeBalancedFromList (toList t) where
    makeBalancedFromList [] = Empty
    makeBalancedFromList (x:[]) = Node 1 Empty x Empty
    makeBalancedFromList (x:xs) =
        let
            half = div (length xs) 2
            listL = (take half xs)
            listR = (drop half xs)
        in makeNode (makeBalancedFromList listL) x (makeBalancedFromList listR)|

exampleTree :: BTree Int
exampleTree =
  Node 6
    ( Node 3
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