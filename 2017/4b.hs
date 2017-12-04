import Data.List
import Data.List.Split
import Data.Set

main = do
    rawInput <- readFile "4.txt"
    let ls = lines rawInput
    let valid = [ v | v <- ls, validPassphrase v]
    putStrLn $ show $ length valid

validPassphrase :: String -> Bool
validPassphrase s = let ws = words s
                        sortedAnagramWords = Data.List.map Data.List.sort ws
                        set = Data.Set.fromList (sortedAnagramWords)
                    in (Data.Set.size set) == (length ws)
