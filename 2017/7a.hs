import qualified Data.Map as Map
import Text.Regex

main = do
    print "hi"

-- make map of child -> parent
-- find the node that has no parent

data Input = Input { name :: String
                   , children :: [String] }

-- fwft (72) -> ktlj, cntj, xhth
inputRegex = mkRegex "([a-z]+) \\(([0-9]+)\\)(.*)"

parseInputLine :: String -> Input
parseInputLine = 
    Input name children
    where 