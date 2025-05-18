data PHeaps a = Empty | Root a [PHeaps a] deriving Show

isPHeap :: Ord a => PHeaps a -> Bool
isPHeap Empty = True
isPHeap (Root _ []) = True
isPHeap (Root a (p@(Root b _):xs)) = (b > a) && (isPHeap p) && (isPHeap (Root a xs))

isPHeap' :: Ord a => PHeaps a -> Bool
isPHeap' Empty = True
isPHeap' (Root r lst) = all (\x -> case x of
    Empty -> True
    Root rInner _ -> r < rInner) lst
    && all isPHeap' lst

mergePheap :: Ord a => PHeaps a -> PHeaps a -> PHeaps a
mergePheap h1 Empty = h1
mergePheap Empty h2 = h2
mergePheap h1@(Root r1 l1) h2@(Root r2 l2)  | r1 < r2 = Root r1 (h2:l1)
                                            | otherwise = Root r2 (h1:l2)

insertPheap :: Ord a => PHeaps a -> a -> PHeaps a
insertPheap heap x = mergePheap heap (Root x [])

concatePHeaps :: Ord a => [PHeaps a ] -> PHeaps a
concatePHeaps [] = Empty
concatePHeaps (x:xs) = mergePheap x (concatePHeaps xs)

delMinPheap :: Ord a => PHeaps a -> Maybe (a, PHeaps a)
delMinPheap Empty = Nothing
delMinPheap (Root r lst) = Just (r, concatePHeaps lst)


testHeap1 :: PHeaps Int
testHeap1 = Root 3 [Root 5 [Root 7 []], Root 8 []]

testHeap2 :: PHeaps Int
testHeap2 =  Root 2
    [ Root 4
        [ Root 6 []
        , Root 7
            [ Root 9 [] ]
        ]
    , Root 5
        [ Root 8 []
        ]
    , Root 10
        [ Root 12 []
        , Root 14
            [ Root 15 [] ]
        ]
    ]