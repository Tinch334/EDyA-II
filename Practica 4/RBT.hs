module RBT (
    Color(..),
    RBT(..),
    memberRBT,
    insertRBT,
    insertRBTOptimized
) where

data Color = R | B
data RBT a = E | T Color (RBT a) a (RBT a)

memberRBT :: Ord a => a -> RBT a -> Bool
memberRBT _ E = False
memberRBT x (T _ l y r) | x == y = True
                        | x < y = memberRBT x l
                        | x > y = memberRBT x r

insertRBT :: Ord a => a -> RBT a -> RBT a
insertRBT e t = makeBlack(insertInner e t) where --Balancing can cause a Red head
    insertInner x E = T R E x E
    insertInner x tree@(T c l y r)  | x < y = balance c (insertInner x l) y r
                                    | x > y = balance c l y (insertInner x r)
                                    | otherwise = tree
    makeBlack E = E
    makeBlack (T _ l y r) = T B l y r

insertRBTOptimized :: Ord a => a -> RBT a -> RBT a
insertRBTOptimized e t = makeBlack(insertInner e t) where
    insertInner x E = T R E x E
    insertInner x tree@(T c l y r)  | x < y = lbalance c (insertInner x l) y r
                                    | x > y = rbalance c l y (insertInner x r)
                                    | otherwise = tree
    makeBlack E = E
    makeBlack (T _ l y r) = T B l y r

lbalance :: Color -> RBT a -> a -> RBT a -> RBT a
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
lbalance c l a r = T c l a r

rbalance :: Color -> RBT a -> a -> RBT a -> RBT a
rbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance c l a r = T c l a r

balance :: Color -> RBT a -> a -> RBT a -> RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r