import qualified Data.Map.Strict as Map
import Debug.Trace

main = do
    putStrLn $ show $ generateMatrix 312051

data Vec2 = Vec2 Int Int deriving (Ord, Show, Eq)

rotateLeft :: Vec2 -> Vec2
rotateLeft (Vec2 x y) = (Vec2 (-y) x)

add :: Vec2 -> Vec2 -> Vec2
add (Vec2 x1 y1) (Vec2 x2 y2) = (Vec2 (x1 + x2) (y1 + y2))

generateMatrix :: Int -> Int
generateMatrix n = generateMatrix' n 1 startDirection startPosition (Map.singleton startPosition 1)

generateMatrix' :: Int -> Int -> Vec2 -> Vec2 -> (Map.Map Vec2 Int) -> Int
generateMatrix' targetNumber currentNumber direction position accum =
    if targetNumber < currentNumber then
        currentNumber
    else
        let leftTurn = rotateLeft direction
            leftPosition = add position leftTurn
            forwardPosition = add position direction
            leftIsEmpty =  not $ Map.member leftPosition accum
            nextPosition = if leftIsEmpty then leftPosition else forwardPosition
            nextDirection = if leftIsEmpty then leftTurn else direction
            nextNumber = sum $ valuesAroundPosition nextPosition accum
            newAccum = Map.insert nextPosition nextNumber accum
        in generateMatrix' targetNumber nextNumber nextDirection nextPosition newAccum

startPosition = Vec2 0 0
startDirection = Vec2 (-1) 0

valuesAroundPosition :: Vec2 -> (Map.Map Vec2 Int) -> [Int]
valuesAroundPosition p m =
    [ Map.findWithDefault 0 v m | v <- surroundingPositions p ]

surroundingPositions :: Vec2 -> [Vec2]
surroundingPositions (Vec2 x y) =
    [
        (Vec2 (x + 1) y),
        (Vec2 (x - 1) y),

        (Vec2 (x + 1) (y + 1)),
        (Vec2 (x - 1) (y - 1)),

        (Vec2 x (y + 1)),
        (Vec2 x (y - 1)),

        (Vec2 (x + 1) (y - 1)),
        (Vec2 (x - 1) (y + 1))
    ]