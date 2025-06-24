import ListSeq as L
import Par

--a
merge :: [Int] -> [Int] -> [Int]
merge s1 s2 | len1 == 0 = s2
            | len2 == 0 = s1
            | otherwise = let
                m1 = div len1 2
                pivot = (L.nthS s1 m1)
                pivotpos = getIndex s2 pivot
                (s1l, s1r) = (L.takeS s1 (m1 - 1), L.dropS s1 (m1 + 1)) 
                (s2l, s2r) = (L.takeS s2 pivotpos, L.dropS s2 pivotpos) in
                    L.appendS (L.appendS (merge s1l s2l) (L.singletonS pivot)) (merge s1r s2r)
            where (len1, len2) = (L.lengthS s1, L.lengthS s2)

getIndex :: [Int] -> Int -> Int
getIndex s e = if (L.lengthS s) == 1
    then 0
    else let
        m = div (L.lengthS s) 2
        pivot = (L.nthS s m) in
            if (e < pivot) then getIndex (L.takeS s m) e else m + getIndex (L.dropS s m) e

--b
sort :: [Int] -> [Int]
sort s  | len == 0 = L.emptyS
        | otherwise = let
            pivot = L.nthS s 0
            (l, r) = L.filterS (<= pivot) (L.dropS s 1) ||| L.filterS (> pivot) s in
                L.appendS (L.appendS (sort l) (L.singletonS pivot)) (sort r)
        where len = L.lengthS s


--c
maxE :: [Int] -> Int
maxE s = if (L.lengthS s) == 0
    then error "Empty sequence"
    else L.reduceS (max) (L.nthS s 0) s

maxS :: [Int] -> Int
maxS s = if (L.lengthS s) == 0
    then error "Empty sequence"
    else let posLst = (L.tabulateS (\n -> (L.nthS s n, n)) (L.lengthS s)) :: [(Int, Int)] in
        snd (L.reduceS (\p1@(e1, _) p2@(e2, _) -> if e1 > e2 then p1 else p2) (L.nthS s 0, 0) posLst)

group :: [Int] -> [Int]
group s = let
    posLst = (L.tabulateS (\n -> (L.nthS s n, n)) (L.lengthS s)) :: [(Int, Int)] in
        L.mapS fst (filterS (\(e, p) -> p == 0 || e /= (L.nthS s (p - 1))) posLst)

collect :: [(Int, String)] -> [(Int, [String])]
collect s = let
    keys = group (sort (L.mapS (\(k, v) -> k) s))
    pairs = L.mapS (\x -> L.filterS (\(k, v) -> x == k) s) keys
    combinedPairs = L.mapS (\lst -> (fst (L.nthS lst 0), L.reduceS L.appendS L.emptyS (L.mapS (\x -> L.singletonS (snd x)) lst))) pairs in
        combinedPairs


lst :: [Int]
lst = [1, 2, 3, 5, 6, 9, 10]


lst2 :: [Int]
lst2 = [5, 7, 9]


lst3 :: [(Int, String)]
lst3 = [(2, "A"), (1, "B"), (1, "C"), (2, "D"), (3, "E"), (1, "F")]