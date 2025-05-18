data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show

eval :: Exp -> Maybe Int
eval (Lit n) = Just n
eval (Add n1 n2) =
    case (eval n1, eval n2) of
        (Just x, Just y) -> Just (x + y)
        _ -> Nothing
eval (Sub n1 n2) =
    case (eval n1, eval n2) of
        (Just x, Just y) -> Just (x - y)
        _ -> Nothing
eval (Prod n1 n2) =
    case (eval n1, eval n2) of
        (Just x, Just y) -> Just (x * y)
        _ -> Nothing
eval (Div n1 n2) = case (eval n1, eval n2) of
        (Just _, Just 0) -> Nothing
        (Just x, Just y) -> Just (div x y)
        _ -> Nothing