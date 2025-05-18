import Data.Char

data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show

eval :: Exp -> Int
eval (Lit n) = n
eval (Add n1 n2) = eval n1 + eval n2
eval (Sub n1 n2) = eval n1 - eval n2
eval (Prod n1 n2) = eval n1 * eval n2
eval (Div n1 n2) = div (eval n1) (eval n2)

getNum :: String -> (String, String)
getNum str = getNumInner "" str where
    getNumInner n [] = (n, [])
    getNumInner n (char:strInner)   | not (isDigit char) = (n, char:strInner)
                                    | otherwise = getNumInner (n++[char]) strInner

parseRPN :: String -> Int
parseRPN str = eval (parseRPN' str [])

parseRPN' :: String -> [Exp] -> Exp
parseRPN' "" stack = head stack
parseRPN' (char:str) stack  | char == ' ' = parseRPN' str stack
                            | isDigit char = let (num, restStr) = getNum (char:str) in parseRPN' restStr (Lit (read num):stack)
                            | otherwise = parseRPN' str ((makeOperation char (head (tail stack)) (head stack)) : (tail (tail stack)))

makeOperation :: Char -> Exp -> Exp -> Exp
makeOperation char e1 e2    | char == '+' = Add e1 e2
                            | char == '-' = Sub e1 e2
                            | char == '*' = Prod e1 e2
                            | char == '/' = Div e1 e2
                            | otherwise = error ("Unknown operator: " ++ [char])
