-- src/Game/State.hs
module Game.State
  ( GameState(..)
  , initState
  , legalMoves
  , applyMove
  ) where

import Game.Board (Board, boardSize, getCell, setCell, emptyBoard)
import Game.Types (Cell(..), Player(..), Pos, opponent)

data GameState = GameState
  { gsBoard :: Board
  , gsTurn  :: Player
  , gsK     :: Int
  } deriving (Eq, Show)

initState :: Int -> Int -> Int -> GameState
initState rows cols k =
  GameState
    { gsBoard = Game.Board.emptyBoard rows cols
    , gsTurn  = P1
    , gsK     = k
    }

legalMoves :: GameState -> [Pos]
legalMoves st =
  let b = gsBoard st
      (rows, cols) = boardSize b
  in [ (r,c)
     | r <- [0..rows-1]
     , c <- [0..cols-1]
     , getCell b (r,c) == Just Empty
     ]

-- Apply a move for the current player
applyMove :: Pos -> GameState -> Maybe GameState
applyMove pos st = do
  b' <- setCell (gsBoard st) pos (gsTurn st)
  pure st { gsBoard = b', gsTurn = opponent (gsTurn st) }