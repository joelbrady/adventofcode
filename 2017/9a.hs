import Debug.Trace

data Tree = Group [Tree] | Garbage deriving Show

scoreTree :: Tree -> Int
scoreTree t = scoreTree' 1 t

scoreTree' :: Int -> Tree -> Int
scoreTree' _ Garbage = 0
scoreTree' depth t = depth + (sum $ map (scoreTree' (depth + 1)) children)
    where (Group children) = t

parse :: String -> Tree
parse s = t
    where (_, [t]) = parse' [] [] s

parse' :: [Tree] -> -> String -> (String, [Tree])
parse' [] [] "" = ("", [])

-- start of a group
parse' stack ('{':cs) = (remainder, ts)
    where (remainder, ts) = parse' (Group []:stack) cs

-- end of a group
parse' (h:stack) ('}':cs) = (remainder, [Group ts])
    where (remainder, ts) = parse' stack cs

-- parse' stack (',':cs) = (remainder, ts)
--     where (remainder, ts) = parse' stack cs

-- only within garbage
--parse' stack t ('!':_:cs) = parse' stack t cs
