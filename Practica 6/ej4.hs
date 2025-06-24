data T a = E | N (T a) a (T a) deriving Show

(|||) :: a -> b -> (a, b)
x ||| y = (x, y)

altura :: T a -> Int
altura E = 0
altura (N l x r ) = 1 + max (altura l) (altura r )

combinar :: T a -> T a -> T a
combinar E E = E
combinar t1 E = t1
combinar E t2 = t2
combinar t1@(N l1 a1 r1) t2 = let l' = (combinar l1 r1) in
    N l' a1 t2

filterT :: (a -> Bool) -> T a -> T a
filterT _ E = E
filterT f (N l a r) = let (l', r') = filterT f l ||| filterT f r in
    if (f a) then N l' a r' else combinar l' r'

quicksortT :: T Int -> T Int
quicksortT E = E
quicksortT (N l a r) = let c = (combinar l r) in
    let (s, b) = (filterT (\x -> x < a) c) ||| (filterT (\x -> x >= a) c) in
        N (quicksortT s) a (quicksortT b)

largeTree :: T Int
largeTree =
  N
    (N
      (N
        (N E 1 E)
        3
        (N E 4 E)
      )
      6
      (N
        (N E 70 E)
        8
        (N E 2 E)
      )
    )
    10
    (N
      (N
        (N E 5 E)
        13
        (N E 14 E)
      )
      11855
      (N
        (N E 16 E)
        18
        (N E 1 E)
      )
    )