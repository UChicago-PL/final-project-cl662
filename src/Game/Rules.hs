-- src/Game/Rules.hs
module Game.Rules
  ( Outcome(..)
  , checkOutcome
  , linesOfLengthK      -- exported for tests / reuse
  , winnerInLines       -- exported for tests / reuse
  ) where

import Game.Board (Board, boardSize, getCell)
import Game.Types (Cell(..), Player(..), Pos)

data Outcome
  = Ongoing
  | Winner Player
  | Draw
  deriving (Eq, Show)

-- checkOutcome function Compute the game outcome for a given k and board.
-- Win condition: k contiguous marks by the same player.
-- Another condition is Draw board full and no winner.
-- Otherwise the output data type Outcome is going to be Ongoing.

checkOutcome :: Int -> Board -> Outcome
checkOutcome k board
  | k <= 0 = Ongoing
  | otherwise =
      case winnerInLines board (linesOfLengthK k board) of
        Just p  -> Winner p
        Nothing ->
          if boardFull board then Draw else Ongoing

boardFull :: Board -> Bool
boardFull b =
  let (rows, cols) = boardSize b
  in all (\pos -> getCell b pos /= Just Empty) [ (r,c) | r <- [0..rows-1], c <- [0..cols-1] ]

dirs :: [(Int, Int)]
dirs =
  [ (0, 1) 
  , (1, 0)
  , (1, 1)
  , (1, -1)
  ]

-- Generate every contiguous lines of length k
-- entirely within bounds, across the four directions.
linesOfLengthK :: Int -> Board -> [[Pos]]
linesOfLengthK k b =
  let (rows, cols) = boardSize b
      inBounds (r,c) = r >= 0 && r < rows && c >= 0 && c < cols
      segmentFrom (r,c) (dr,dc) =
        let seg = [ (r + i*dr, c + i*dc) | i <- [0..k-1] ]
        in if all inBounds seg then Just seg else Nothing
  in
    [ seg
    | r <- [0..rows-1]
    , c <- [0..cols-1]
    , d <- dirs
    , Just seg <- [segmentFrom (r,c) d]
    ]

-- If any segment is a winning segment, return the winning player.
winnerInLines :: Board -> [[Pos]] -> Maybe Player
winnerInLines b = go
  where
    go [] = Nothing
    go (seg:segs) =
      case winnerOnSegment b seg of
        Just p  -> Just p
        Nothing -> go segs

-- Winner on a single segment: all cells taken by same player.
winnerOnSegment :: Board -> [Pos] -> Maybe Player
winnerOnSegment b seg =
  case traverse (getCell b) seg of
    Nothing -> Nothing
    Just cells ->
      case cells of
        [] -> Nothing
        (Taken p : rest)
          | all (== Taken p) rest -> Just p
          | otherwise             -> Nothing
        _ -> Nothing