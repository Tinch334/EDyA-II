import ListSeq as L


v :: (Int, Int, Int, Int, Int)
v = (0, 0, 0, 0, 0)

base :: Int -> (Int, Int, Int, Int, Int)
base v = (1, 1, 1, v, v)

combine (m, p, s, l, r) (m', p', s', l', r') = let res = r < l' in
                                                (if res then maximum [m, m', s + p'] else (max m m'),
                                                if res && p == s then p' + p else p,
                                                if res && s' == p' then s + s' else s',
                                                l,
                                                r')

sccml s = let (r, _, _, _, _) = (L.reduceS combine v (L.mapS base s)) in r - 1


sccmlScan :: [Int] -> Int
sccmlScan s = let
    cLst = (L.tabulateS (\n -> if (L.nthS s n) < (L.nthS s (n + 1)) then 1 else minBound) ((L.lengthS s) - 1)) :: [Int]
    (sl, l) = L.scanS (+) 0 cLst
    sumLst = L.appendS sl (L.singletonS l)
    (ml, l') = L.scanS min maxBound sumLst
    minlLst = L.appendS ml (L.singletonS l')
    posSS = (L.tabulateS (\n -> (L.nthS sumLst n) - (L.nthS minlLst n)) ((L.lengthS s) - 1)) :: [Int] in
        L.reduceS (max) minBound posSS


lst :: [Int]
lst = [1, 3, 3, 3, 3, 3, 3, 3, 3, 3]


lst2 :: [Int]
lst2 = [5, 6, 2, 3, 5, 1, 9]


lst3 :: [Int]
lst3 = [1, 4, 6, 7, 8, 11, 12, 3]