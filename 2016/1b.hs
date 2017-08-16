{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Read as R

data Direction = L | R deriving Show
data Orientation = N | S | E | W deriving Show
data Instruction = Instruction Direction Int deriving Show
data Coordinates = Coordinates (Int, Int) deriving (Show, Eq, Ord)
data State = State Coordinates Orientation (Set.Set Coordinates) (Maybe Coordinates) deriving Show

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
textToInt text = case R.decimal text of Right (n, _) -> n
                                        Left err -> error err

distanceFromOrigin :: Coordinates -> Int
distanceFromOrigin (Coordinates (x, y)) = abs(x) + abs(y)

-- TODO use vector instead
rotate :: Orientation -> Direction -> Orientation
rotate N L = W
rotate N R = E
rotate E L = N
rotate E R = S
rotate S L = E
rotate S R = W
rotate W L = S
rotate W R = N

moveForward :: Coordinates -> Orientation -> Int -> Coordinates
moveForward (Coordinates (x, y)) N n = Coordinates (x, y + n)
moveForward (Coordinates (x, y)) S n = Coordinates (x, y - n)
moveForward (Coordinates (x, y)) E n = Coordinates (x + n, y)
moveForward (Coordinates (x, y)) W n = Coordinates (x - n, y)

f :: State -> Instruction -> State
f (State c o v m) (Instruction d n) =
    case m of Just _ -> State c o v m
              Nothing ->
                    let newOrientation = rotate o d
                        newCoordinates = moveForward c newOrientation n
                        newVisitedSet  = Set.insert newCoordinates v
                        maybeVisitedTwice = if Set.member newCoordinates v then
                                                Just newCoordinates
                                            else
                                                Nothing
                    in State newCoordinates newOrientation newVisitedSet maybeVisitedTwice

initialCoordinates = Coordinates (0, 0)
initialDirection   = N
initialVisitedSet  = Set.empty
startState         = State initialCoordinates initialDirection initialVisitedSet Nothing
inputFilename      = "1b.input"

rawInputStringToTokens :: Text.Text -> [Text.Text]
rawInputStringToTokens text = Text.splitOn "," text

parseInput :: Text.Text -> [Instruction]
parseInput s = map (instruction . Text.strip) (rawInputStringToTokens s)

main :: IO ()
main = do
    rawInput <- readFile inputFilename
    let moves = parseInput $ Text.pack rawInput
    let endPosition = finalPosition startState moves
    let distance = distanceFromOrigin $ getCurrentCoordinates endPosition
    let solution = getVistedSet endPosition
    putStrLn $ show solution

finalPosition :: State -> [Instruction] -> State
finalPosition start moves = foldl f start moves

getCurrentCoordinates :: State -> Coordinates
getCurrentCoordinates (State c _ _ _) = c

getSolution :: State -> Maybe Coordinates
getSolution (State _ _ _ m) = m

getVistedSet :: State -> Set.Set Coordinates
getVistedSet (State _ _ s _) = s
