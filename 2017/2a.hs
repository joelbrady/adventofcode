import Data.List.Split

main = do
    input <- readFile "2.txt"
    let ls = lines input
    let sns = map words ls
    let ns = [ map read l | l <- sns ]
    let mms = [ (minimum l, maximum l) | l <- ns ]
    let ss =  [ diff t | t <- mms ]
    putStrLn $ show $ sum ss

diff :: (Int, Int) -> Int
diff (a, b) = abs $ a - b