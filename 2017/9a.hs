import Debug.Trace

matches :: String -> (String, Int)
matches ""     = ("", 0)
matches (s:"") = (s:"", 0)
matches (s:ss) = trace ss $ case leftOver of "" -> ("", 0)
                                             x:xs -> if s `match` x then (xs, c + 1) else (x:xs, c)
    where (leftOver, c) = matches ss

match :: Char -> Char -> Bool
match '(' ')' = True
match _ _ = False

data Tree a = Leaf a | Node a (Tree a) (Tree a)

insert :: Ord a => a -> Tree a -> Tree a