import Debug.Trace
import qualified Data.Set as Set
import qualified Data.List as List

main = do
    print $ if test then "test passed" else "test failed"


testInput = [0, 2, 7, 0]
testResult = 5

test :: Bool
test = runTest testInput testResult

runTest :: [Int] -> Int -> Bool
runTest testInput testResult = (run testInput) == testResult

run :: [Int] -> Int
run list = run' list 0 Set.empty

run' :: [Int] -> Int -> Set.Set [Int] -> Int
run' list count seen
    | newList `Set.member` seen = count + 1
    | otherwise                 = run' newList (count + 1) newSeen
    where newList = redistribute list
          newSeen = newList `Set.insert` seen

redistribute :: [Int] -> [Int]
redistribute list = list