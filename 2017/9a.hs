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
    where (_, t) = parse' [] [] s

parse' :: [Char] -> [Tree] -> String -> (String, Tree)
parse' _ [] "" = ("", Garbage)
parse' _ ts "" = ("", Group ts)
parse' stack children ('{':cs) = (remainder, Group (children ++ (nodes t)))
    where (remainder, t) = parse' ('{':stack) [] cs
parse' ('{':stack) t ('}':cs) = (cs, Group t)
--parse' stack t ('!':_:cs) = parse' stack t cs

nodes :: Tree -> [Tree]
nodes (Group n) = n
nodes _ = []