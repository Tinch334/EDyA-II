import ListSeq as L
import Par

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

collect :: [(Int, Int)] -> [(Int, [Int])]
collect s = let
    keys = group (sort (L.mapS (\(k, v) -> k) s))
    pairs = L.mapS (\x -> L.filterS (\(k, v) -> x == k) s) keys
    combinedPairs = L.mapS (\lst -> (fst (L.nthS lst 0), L.reduceS L.appendS L.emptyS (L.mapS (\x -> L.singletonS (snd x)) lst))) pairs in
        combinedPairs


mapCollectReduce apv red s = let
    pairs = L.mapS apv s
    groups = collect pairs in
        L.mapS red groups


getInfo :: [(String, [Int])] -> [(Int, Int)]
getInfo s = mapCollectReduce avgCond redFun s

avgCond :: (String, [Int]) -> (Int, Int)
avgCond (_, notes)  | avg >= 70 = (0, avg)
                    | avg >= 50 = (1, avg)
                    | otherwise = (2, avg)
                    where avg = div (L.reduceS (+) 0 notes) (L.lengthS notes)

redFun :: (Int, [Int]) -> (Int, Int)
redFun (_, avgs) = (L.lengthS avgs, L.reduceS (max) (L.nthS avgs 0) avgs)


alumnos :: [(String, [Int])]
alumnos = L.fromList
  [ ("Alice",   L.fromList [85, 92, 78])
  , ("Bob",     L.fromList [67, 10, 88, 100])
  , ("Charlie", L.fromList [4, 4, 4])
  , ("Diana",   L.fromList [55, 60, 72, 81, 95])
  , ("Eve",     L.fromList [100, 100, 99])
  ]