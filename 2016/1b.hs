{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import qualified Debug.Trace as Trace

data Direction = L | R deriving Show
data Instruction = Instruction Direction Int deriving Show
data State = State Vector Vector (Set.Set Vector) deriving Show
data Vector = Vector Int Int deriving (Show, Eq, Ord)

direction :: Char -> Direction
direction c
    | c == 'L' = L
    | c == 'R' = R
    | otherwise = error "not implemented"

instruction :: Text.Text -> Instruction
instruction text =
    let d = direction $ Text.head text
        n = textToInt $ Text.tail text
    in Instruction d n

textToInt :: Text.Text -> Int
textToInt text = case Read.decimal text of Right (n, _) -> n
                                           Left err -> error err

distanceFromOrigin :: Vector -> Int
distanceFromOrigin (Vector x y) = abs(x) + abs(y)

rotate :: Vector -> Direction -> Vector
rotate o R = rotateRight o
rotate o L = rotateLeft o

rotateRight :: Vector -> Vector
rotateRight (Vector x y) = Vector y (-x)

rotateLeft :: Vector -> Vector
rotateLeft = rotateRight . rotateRight . rotateRight

add :: Vector -> Vector -> Vector
add (Vector a1 b1) (Vector a2 b2) = Vector (a1 + a2) (b1 + b2)

moveForward :: Vector -> Vector -> Int -> Set.Set Vector-> (Vector, Set.Set Vector, Maybe Vector)
moveForward coordinates _ 0 visitedSet = (coordinates, visitedSet, Nothing)
moveForward start orientation steps visitedSet =
    let newPosition = start `add` orientation
    in
        if Set.member newPosition visitedSet then
            (newPosition, visitedSet, Just newPosition)
        else
            moveForward newPosition orientation (steps - 1) (Set.insert newPosition visitedSet)

f :: State -> [Instruction] -> State
f s [] = s
f (State coordinates orientation visited) ((Instruction d n):is) =
    let newOrientation = rotate orientation d
        (newCoordinates, newVisitedSet, possibleSolution) = moveForward coordinates newOrientation n visited
        nextState = State newCoordinates newOrientation newVisitedSet
    in case possibleSolution of Just _  -> nextState
                                Nothing -> f nextState is

initialCoordinates = Vector 0 0
initialDirection   = Vector 0 1
initialVisitedSet  = Set.empty
startState         = State initialCoordinates initialDirection initialVisitedSet
inputFilename      = "1b.input"

rawInputStringToTokens :: Text.Text -> [Text.Text]
rawInputStringToTokens text = Text.splitOn "," text

parseInput :: Text.Text -> [Instruction]
parseInput s = map (instruction . Text.strip) (rawInputStringToTokens s)

main :: IO ()
main = do
    rawInput <- readFile inputFilename
    let moves = parseInput $ Text.pack rawInput
    let endPosition = f startState moves
    let distance = distanceFromOrigin $ getCurrentCoordinates endPosition
    putStrLn $ show distance

getCurrentCoordinates :: State -> Vector
getCurrentCoordinates (State c _ _) = c

getVistedSet :: State -> Set.Set Vector
getVistedSet (State _ _ s) = s
