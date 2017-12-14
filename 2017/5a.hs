import Debug.Trace


main :: IO ()
main = do
    rawLines <- fmap lines $ readFile "5.txt"
    let ram = [read s | s <- rawLines]
    putStrLn $ show $ run $ initialState ram

data State = State {pc :: Int, ram :: [Int], count :: Int} deriving (Show)

initialState :: [Int] -> State
initialState ram = State { pc = 0, ram = ram, count = 0 }

testRam :: [Int]
testRam = [0, 3, 0, 1, -3]

run :: State -> Int
run state
    | pc > maxMemoryAddress || pc < 0 = count -- we're out of bounds we can stop
    | otherwise = run (State nextPc newRam (count + 1))
    where (State pc ram count) = state
          maxMemoryAddress = (length ram) - 1
          nextPc = pc + (ram !! pc)
          newRam = replaceIndex ram pc ((ram !! pc) + 1)

replaceIndex :: [Int] -> Int -> Int -> [Int]
replaceIndex xs i x = replaceIndex' xs 0 i x

replaceIndex' :: [Int] -> Int -> Int -> Int -> [Int]
replaceIndex' (x:xs) currentIndex indexToReplace newElement
    | currentIndex == indexToReplace = newElement:xs
    | otherwise                      = x:(replaceIndex' xs (currentIndex + 1) indexToReplace newElement)

-- dumb implementation
-- replaceIndex xs i x = take i xs ++ [x] ++ drop (i+1) xs
