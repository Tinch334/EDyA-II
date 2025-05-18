data Bin a = Leaf | Node (Bin a) a (Bin a)


complete :: a -> Int -> Bin a
complete x d = let subTree = complete x (d - 1) in Node subTree x subTree