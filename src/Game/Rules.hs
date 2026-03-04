module Game.Rules
  ( Outcome(..)
  , checkOutcome
  ) where

import Game.Board (Board)
import Game.Types (Player(..))

data Outcome
  = Ongoing
  | Winner Player
  | Draw
  deriving (Eq, Show)

checkOutcome :: Int -> Board -> Outcome
checkOutcome _k _board = Ongoing