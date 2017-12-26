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

parse' :: [Char] -> [Tree] -> String -> (String, [Tree])
parse' [] [] "" = ("", [])

-- start of a group
parse' stack siblings ('{':cs) = (remainder, ts)
    where (remainder, ts) = parse' ('{':stack) [] cs

-- end of a group
parse' ('{':stack) siblings ('}':cs) = (remainder, [Group ts])
    where (remainder, ts) = parse' stack [] cs

parse' stack siblings (',':cs) = (remainder, siblings ++ ts)
    where (remainder, ts) = parse' stack ts cs

-- only within garbage
--parse' stack t ('!':_:cs) = parse' stack t cs
