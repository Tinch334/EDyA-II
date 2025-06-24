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
sort :: [Char] -> [Char]
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

group :: [Char] -> [Char]
group s = let
    posLst = (L.tabulateS (\n -> (L.nthS s n, n)) (L.lengthS s)) :: [(Char, Int)] in
        L.mapS fst (filterS (\(e, p) -> p == 0 || e /= (L.nthS s (p - 1))) posLst)

collect :: [(Char, Int)] -> [(Char, [Int])]
collect s = let
    keys = group (sort (L.mapS (\(k, v) -> k) s))
    pairs = L.mapS (\x -> L.filterS (\(k, v) -> x == k) s) keys
    combinedPairs = L.mapS (\lst -> (fst (L.nthS lst 0), L.reduceS L.appendS L.emptyS (L.mapS (\x -> L.singletonS (snd x)) lst))) pairs in
        combinedPairs


mapCollectReduce apv red s = let
    pairs = L.joinS (L.mapS apv s)
    groups = collect pairs in
        L.mapS red groups

countChar :: [[Char]] -> [(Char, Int)]
countChar s = mapCollectReduce (\str -> L.mapS (\c -> (c, 1)) str) redx s

redx :: (Char, [Int]) -> (Char, Int)
redx (c, lst) = (c, L.reduceS (+) 0 lst)

huffman :: [[Char]] -> [(Int, [Char])]
huffman s = let
    cc = L.mapS (\(c, n) -> (n, c)) (countChar s) in
        collect cc


testStrings = [
    "Virtual machines offer an interesting solution to a problem that has long plagued users, especially users of open source software: how to install new appli- cation programs. The problem is that many applications are dependent on numerous other applications and libraries, which are themselves dependent on a host of other software packages, and so on. Furthermore, there may be dependencies on particular versions of the compilers, scripting languages, and the operating system.",

    "They pre-emptively evacuated some regions in the South East so that civilian evacuations would not foul up British logistics and manoeuvre. The population was told that evacuations would take a lower priority and, if not already evacuated by the time of an invasion, they should stay where they were unless ordered to retreat.",

    "A sample of the signal at the collector of TR is peak rectified by D, DB and C to turn off TR. If for any reason (i.e. no input signal) the trigger stage is not producing pulses then TR conducts, causing the timebase to free run. Immediately trigger pulses are produced, TR is turned off and the timebase reverts to the triggered state.",

    "British officials found Koops analysis highly impressive. Arthur Kellas, a British diplomat, had acquired a copy of the study and in his forwarding letter observed that it was a model of what these things should be. Treweeks, with the Defence Intelligence Staff, later commended the Canadian intelligence study, declaring that we agreed with what is said and with the conclusions. Apparently the report had not been shared with U.S. intelligence",

    "The meetings of the General Conference of Representatives of the Members shall be held from time to time as occasion may require, and at least once in every year. It shall be composed of four Representatives of each of the Members, of whom two shall be Government Delegates and the two others shall be Delegates representing respectively the employers and the workers of each of the Members",

    "The period of office of the members of the Governing Body will be three years. The method of filling vacancies and other similar questions may be determined by the Governing Body subject to the approval of the Conference.",

    "It is in Latin administrative documents of the ninth century that written GalicianPortuguese words and phrases are first recorded. This phase is known as Proto-Portuguese, which lasted from the ninth century until the twelfth-century independence of the County of Portugal from the Kingdom of Leon, which had by then assumed reign over Galicia.",
    
    "In reconstructing the history of the Indo-European languages and the form of the Proto-Indo-European language, some languages have been of particular importance. These generally include the ancient Indo-European languages that are both well-attested and documented at an early date, although some languages from later periods are important if they are particularly linguistically conservative (most notably, Lithuanian). Early poetry is of special significance because of the rigid poetic meter normally employed, which makes it possible to reconstruct a number of features (e.g. vowel length) that were either unwritten or corrupted in the process of transmission down to the earliest extant written manuscripts.",

    "After another revolt in June, the constitution was suspended and power passed from the National Convention to the Committee of Public Safety. About six thousand people were executed in a Reign of Terror, which ended in July. Weakened by external threats and internal opposition, the Republic was replaced by the Directory. Four years later, the Consulate seized power in a coup led by Napoleon.",

    "It was the twelfth day of August, in the ninth consulship of Diocletian and the eighth of Maximian, when the governor Calvisianus said to Euplius under torture: What now do you repeat with regard to the things you admitted in your confession?",

    "abcdefghijklmnopqrstuvwxtz ,."
    ]