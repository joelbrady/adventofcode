import qualified Data.Map.Strict as Map
import Debug.Trace

main = do
    putStrLn $ show $ manhattan $ getCoords 312051

data Vec2 = Vec2 Int Int deriving (Ord, Show, Eq)

rotateLeft :: Vec2 -> Vec2
rotateLeft (Vec2 x y) = (Vec2 (-y) x)

add :: Vec2 -> Vec2 -> Vec2
add (Vec2 x1 y1) (Vec2 x2 y2) = (Vec2 (x1 + x2) (y1 + y2))

generateMatrix :: Int -> Map.Map Vec2 Int
generateMatrix n = generateMatrix' n 1 startDirection startPosition (Map.singleton startPosition 1)

generateMatrix' :: Int -> Int -> Vec2 -> Vec2 -> (Map.Map Vec2 Int) -> (Map.Map Vec2 Int)
generateMatrix' targetNumber currentNumber direction position accum =
    if targetNumber == currentNumber then
        accum
    else
        let nextNumber = currentNumber + 1
            leftTurn = rotateLeft direction
            leftPosition = add position leftTurn
            forwardPosition = add position direction
            leftIsEmpty =  not $ Map.member leftPosition accum
            nextPosition = if leftIsEmpty then leftPosition else forwardPosition
            nextDirection = if leftIsEmpty then leftTurn else direction
            newAccum = Map.insert nextPosition nextNumber accum
        in generateMatrix' targetNumber nextNumber nextDirection nextPosition newAccum

startPosition = Vec2 0 0
startDirection = Vec2 (-1) 0

manhattan :: Vec2 -> Int
manhattan (Vec2 x y) = abs(x) + abs(y)

getCoords :: Int -> Vec2
getCoords n = fst $ Map.elemAt 0 $ Map.filter (\v -> v == n) $ generateMatrix n