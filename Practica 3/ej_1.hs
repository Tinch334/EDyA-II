type Color = (Int, Int, Int)

mezclar :: Color -> Color -> Color
mezclar (r1, g1, b1) (r2, g2, b2) = (avg r1 r2, avg g1 g2, avg b1 b2) where
    avg x y = div (x + y) 2

data Color' = RGB {
    red :: Int,
    green :: Int,
    blue :: Int
}

mezclar' :: Color' -> Color' -> Color'
mezclar' c1 c2 = RGB {
    red = avg (red c1) (red c2),
    green = avg (green c1) (green c2),
    blue = avg (blue c1) (blue c2)
} where
    avg x y = div (x + y) 2