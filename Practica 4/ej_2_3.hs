data Bin a = Leaf | Node (Bin a) a (Bin a) deriving Show

maximumBST :: Ord a => Bin a -> a
maximumBST (Node _ x Leaf) = x
maximumBST (Node _ _ r) = maximumBST r

minimumBST :: Ord a => Bin a -> a
minimumBST (Node Leaf x _) = x
minimumBST (Node l _ _) = minimumBST l

checkBST :: Ord a => Bin a -> Bool
checkBST Leaf = True
checkBST (Node Leaf x Leaf) = True
checkBST (Node Leaf x r) = (x < (minimumBST r)) && (checkBST r)
checkBST (Node l x Leaf) = (x >= (maximumBST l)) && (checkBST l)
checkBST (Node l x r) = (x >= (maximumBST l)) && (x < (minimumBST r)) && (checkBST l) && (checkBST r)

splitBST :: Ord a => Bin a -> a -> (Bin a, Bin a)
splitBST (Node l n r) x = if (x > n) then splitBSTInner r x (Node l n Leaf) Leaf else splitBSTInner l x Leaf (Node Leaf n r) where
    splitBSTInner :: Ord a => Bin a -> a -> Bin a -> Bin a -> (Bin a, Bin a)
    splitBSTInner Leaf _ smaller bigger = (smaller, bigger)
    splitBSTInner (Node l' n' r') x' smaller bigger = if (x' > n') then splitBSTInner r' x' (Node smaller n' l') bigger else splitBSTInner l' x' smaller (Node r' n' bigger)

insertBST :: Ord a => Bin a -> a -> Bin a
insertBST Leaf x = Node Leaf x Leaf
insertBST tree@(Node l y r) x   | x > y = Node l y (insertBST r x)
                                | x < y = Node (insertBST l x) y r
                                | otherwise = tree

--An alternative version could use an inner function taking a list with the elements of the second tree in inorder, it would be more efficient.
joinBST :: Ord a => Bin a -> Bin a -> Bin a
joinBST t1 Leaf = t1
joinBST t1 (Node l x r) = joinBST (insertBST t1 x) (joinBST l r)

memberBST :: Ord a => a -> Bin a -> Bool
memberBST x tree = memberBSTInner Nothing tree where
    memberBSTInner Nothing Leaf = False
    memberBSTInner (Just c) Leaf = x == c
    memberBSTInner c (Node l b r) = if x <= b then memberBSTInner (Just b) l else memberBSTInner c r


tree1 :: Bin Int
tree1 =
  Node
    ( Node
        ( Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf) )
        4
        ( Node (Node Leaf 5 Leaf) 6 (Node Leaf 7 Leaf) )
    )
    8
    ( Node
        ( Node (Node Leaf 9  Leaf) 10 (Node Leaf 11 Leaf) )
        12
        ( Node (Node Leaf 13 Leaf) 14 (Node Leaf 15 Leaf) )
    )

tree2 :: Bin Int
tree2 =
  Node
    ( Node
        (Node Leaf 21 Leaf)
        22
        (Node (Node Leaf 23 Leaf) 24 Leaf)
    )
    25
    ( Node
        (Node (Node Leaf 27 Leaf) 28 (Node Leaf 29 Leaf))
        30
        (Node Leaf 35 Leaf)
    )