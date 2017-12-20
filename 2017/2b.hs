import Data.List.Split

main = do
    input <- readFile "2.txt"
    let ns = getInput input
    putStrLn $ show $ fileChecksum ns


-- returns true of the larger integer is divisible by the smaller integer
divisible :: (Int, Int) -> Bool
divisible (a, b) = 
    if a > b
    then a `mod` b == 0
    else b `mod` a == 0

-- returns the larger integer divided by the smaller integer
quotient :: (Int, Int) -> Int
quotient (a, b) =
    if a > b
    then a `div` b
    else b `div` a

-- converts raw input from file in to a matrix of integers
getInput :: String -> [[Int]]
getInput s = [ map read $ words line | line <- lines s ]

lineChecksum :: [Int] -> Int
lineChecksum ns = quotient $ [ (a, b) | a <- ns, b <- ns, a /= b, divisible (a, b) ] !! 0

fileChecksum :: [[Int]] -> Int
fileChecksum m = sum $ map lineChecksum m
