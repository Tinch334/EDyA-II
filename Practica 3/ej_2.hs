data Line = L {
    chars :: String,
    cursorPos :: Int
} deriving Show

empty :: Line
empty = L {
    chars = "",
    cursorPos = 0
}

moveLeft :: Line -> Line
moveLeft l = L {
    chars = (chars l),
    cursorPos = max 0 (cursorPos l - 1)
}

moveRight :: Line -> Line
moveRight l = L {
    chars = c,
    cursorPos = min (length c) (cursorPos l + 1)
} where
    c = chars l

moveStart :: Line -> Line
moveStart l = L {
    chars = (chars l),
    cursorPos = 0
}

moveEnd :: Line -> Line
moveEnd l = L {
    chars = c,
    cursorPos = length c
} where
    c = chars l


insert :: Char -> Line -> Line
insert char l = L {
    chars = insertInner char (chars l) (cursorPos l),
    cursorPos = cursorPos l + 1
} where
    insertInner c [] _ = [c]
    insertInner c xs 0 = c:xs
    insertInner c (x:xs) cnt = x : insertInner c xs (cnt - 1)

delete :: Line -> Line
delete l = let cPos = (cursorPos l) in
    if cPos == 0 then
    l else
        L {
        chars = removeInner (chars l) (cPos - 1),
        cursorPos = cPos - 1
    } where    
        removeInner [] _ = []
        removeInner (x:xs) 0 = xs
        removeInner (x:xs) cnt = x:(removeInner xs (cnt - 1))

testLine :: Line
testLine = L {
    chars = "Hola capo, como andas?",
    cursorPos = 3
}