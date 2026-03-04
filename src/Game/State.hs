module Game.State
  ( GameState(..)
  , initState
  ) where

import Game.Board (Board, emptyBoard)
import Game.Types (Player(..))

data GameState = GameState
  { gsBoard  :: Board
  , gsTurn   :: Player
  , gsK      :: Int
  } deriving (Eq, Show)

initState :: Int -> Int -> Int -> GameState
initState rows cols k =
  GameState
    { gsBoard = emptyBoard rows cols
    , gsTurn  = P1
    , gsK     = k
    }