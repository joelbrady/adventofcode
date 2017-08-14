-- test comment

import qualified Data.Text as T
import qualified Data.Text.Read as R
    
data Direction = L | R deriving Show
data Orientation = N | S | E | W deriving Show
data Instruction = Instruction Direction Int deriving Show
data Position = Position (Int, Int) Orientation deriving Show

direction :: Char -> Direction
direction 'L' = L
direction 'R' = R

instruction :: T.Text -> Instruction
instruction text =
    let d = direction $ T.index text 0
        n = read $ (case T.stripPrefix (T.pack [(T.index text 0)]) text of Just t -> T.unpack t
                                                                           Nothing -> "-10000")
    in Instruction d n

distanceFromOrigin :: Position -> Int
distanceFromOrigin p = case p of Position (x, y) _ -> abs(x) + abs(y)

rotate :: Orientation -> Direction -> Orientation
rotate N L = W
rotate N R = E
rotate E L = N
rotate E R = S
rotate S L = E
rotate S R = W
rotate W L = S
rotate W R = N

moveForward :: Position -> Int -> Position
moveForward (Position (x, y) N) n = Position (x, y + n) N
moveForward (Position (x, y) S) n = Position (x, y - n) S
moveForward (Position (x, y) E) n = Position (x + n, y) E
moveForward (Position (x, y) W) n = Position (x - n, y) W

f :: Position -> Instruction -> Position
f (Position (x, y) o) (Instruction d n) =
    let newOrientation = rotate o d
    in moveForward (Position (x, y) newOrientation) n

start = Position (0, 0) N
inputFilename = "1.input"

main = do
    rawInput <- readFile inputFilename
    let rawMoves = T.splitOn (T.pack ",") $ T.pack rawInput
    let cleanMoves = map T.strip rawMoves
    let moves = map instruction cleanMoves
    let endPosition = finalPosition start moves
    let distance = distanceFromOrigin endPosition
    putStrLn $ show distance

finalPosition :: Position -> [Instruction] -> Position
finalPosition start moves = foldl f start moves