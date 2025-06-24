module ListSeq (emptyS, singletonS, lengthS, nthS, tabulateS, mapS, filterS, appendS, takeS, dropS, showtS, showlS, joinS, reduceS, scanS, fromList) where

import Par
import Seq

tst f n = tstI 0 where
        tstI n' | n' >= n = []
                | otherwise = let (e, r) = f n' ||| tstI (n' + 1) in e:r

instance Seq [] where
    emptyS = []

    singletonS x = [x]

    lengthS s = Prelude.length s

    nthS s n = s Prelude.!! n

    tabulateS f n = tabulateInner 0 where
        tabulateInner n'    | n' >= n = []
                            | otherwise = let (e, r) = f n' ||| tabulateInner (n' + 1) in e:r

    mapS _ [] = []
    mapS f (x:xs) = let (e, r) = f x ||| mapS f xs in e:r

    filterS _ [] = []
    filterS f (x:xs) = let (eval, r) = f x ||| filterS f xs in
        if eval == True then x:r else r

    appendS s1 s2 = s1 ++ s2

    takeS s n = take n s

    dropS s n = drop n s

    showtS [] = EMPTY
    showtS [x] = ELT x
    showtS s = let mid = div (lengthS s) 2 in NODE (takeS s mid) (dropS s mid)

    showlS [] = NIL
    showlS (x:xs) = CONS x xs

    joinS [] = []
    joinS (xs:xss) = appendS xs (joinS xss)

    reduceS _ b [] = b
    reduceS f b (x:xs) = f x (reduceS f b xs)

    scanS f b s = let (total, lst) = scanSInner b s in (takeS total ((lengthS total) - 1) ,lst)where
        scanSInner b' [] = ([], b')
        scanSInner b' (x:xs) = let 
            newB = f b' x
            (rest, end) = scanSInner newB xs in
                (newB:rest, end)

    fromList xs = xs