import ListSeq as L

multiplos :: [Int] -> Int
multiplos s = let
    isDiv = (L.tabulateS (\n -> (L.reduceS (+) 0 (L.mapS (\x -> if mod (nthS s n) x == 0 then 1 else 0) (L.dropS s (n + 1))) ) ) (L.lengthS s)) :: [Int] in
    L.reduceS (+) 0 isDiv

lst :: [Int]
lst = [12, 4, 6, 3, 2]


lst2 :: [Int]
lst2 = [4, 6, 2]


lst3 :: [Int]
lst3 = [1, 2, 3, 4, 5]