matrixMult :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
matrixMult (a, b, c, d) (e, f, g, h) = (a*e + b*g, a*f + b*h, c*e + d*g, c*f + d*h)

matrixMult' :: (Int, Int, Int, Int) -> Int -> (Int, Int, Int, Int)
matrixMult' _ 0 = (1, 0, 0, 1)
matrixMult' m n = matrixMult m (matrixMult' m (n - 1))

tabulate :: Int -> (Int -> a) -> [a]
tabulate 0 _ = []
tabulate n f = f n : tabulate (n - 1) f

third :: (Int, Int, Int, Int) -> Int
third (_, _, c, _) = c

fibSeq :: Int -> [Int]
fibSeq n = tabulate n (\i -> third (matrixMult' (1, 1, 1, 0) i))