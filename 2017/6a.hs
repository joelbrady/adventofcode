import Debug.Trace
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Sequence as Seq

main = do
    print $ if test then "test passed" else "test failed"


testInput = [0, 2, 7, 0]
testResult = 5

test :: Bool
test = runTest testInput testResult

runTest :: [Int] -> Int -> Bool
runTest testInput testResult = (run testInput) == testResult

run :: [Int] -> Int
run list = run' (Seq.fromList list) 0 Set.empty

run' :: Seq.Seq Int -> Int -> Set.Set (Seq.Seq Int) -> Int
run' list count seen
    | newList `Set.member` seen = count + 1
    | otherwise                 = run' newList (count + 1) newSeen
    where newList = redistribute list
          newSeen = newList `Set.insert` seen

redistribute :: Seq.Seq Int -> Seq.Seq Int
redistribute list = redistribute' n i newList
    where n = maximum list
          i = case Seq.elemIndexL n list of Just i' -> i'
          newList = Seq.adjust (\_ -> 0) i list

redistribute' :: Int -> Int -> Seq.Seq Int -> Seq.Seq Int
redistribute' 0 _ seq = seq
redistribute' n i seq = redistribute' (n-1) newIndex $ Seq.adjust (+1) (i+1) seq
    where newIndex = (i + 1) `mod` (Seq.length seq)
