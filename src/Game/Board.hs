module Game.Board
  ( Board
  , emptyBoard
  , boardSize
  , inBounds
  , getCell
  , setCell
  ) where

import Game.Types (Cell(..), Pos, Player)

newtype Board = Board { unBoard :: [[Cell]] }
  deriving (Eq, Show)

emptyBoard :: Int -> Int -> Board
emptyBoard rows cols = Board (replicate rows (replicate cols Empty))

boardSize :: Board -> (Int, Int)
boardSize (Board rows) =
  case rows of
    []    -> (0, 0)
    r : _ -> (length rows, length r)

inBounds :: Board -> Pos -> Bool
inBounds b (r, c) =
  let (rows, cols) = boardSize b
  in  r >= 0 && r < rows && c >= 0 && c < cols

getCell :: Board -> Pos -> Maybe Cell
getCell b@(Board rows) (r, c)
  | not (inBounds b (r, c)) = Nothing
  | otherwise               = Just ((rows !! r) !! c)

setCell :: Board -> Pos -> Player -> Maybe Board
setCell b@(Board rows) (r, c) p = do
  cell <- getCell b (r, c)
  case cell of
    Taken _ -> Nothing
    Empty   ->
      let newRows =
            [ if i /= r then row
              else [ if j /= c then x else Taken p
                   | (j, x) <- zip [0..] row
                   ]
            | (i, row) <- zip [0..] rows
            ]
      in Just (Board newRows)