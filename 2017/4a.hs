import Data.List.Split
import Data.Set

main = do
    rawInput <- readFile "4.txt"
    let ls = lines rawInput
    let valid = [ v | v <- ls, validPassphrase v]
    putStrLn $ show $ length valid

validPassphrase :: String -> Bool
validPassphrase s = let ws = words s
                        set = Data.Set.fromList (ws)
                    in (Data.Set.size set) == (length ws)
