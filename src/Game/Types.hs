module Game.Types
  ( Player(..)
  , Cell(..)
  , Pos
  , opponent
  ) where

data Player = P1 | P2
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

opponent :: Player -> Player
opponent P1 = P2
opponent P2 = P1

data Cell = Empty | Taken Player
  deriving (Eq, Ord, Show, Read)

-- (row, col), 0-indexed
type Pos = (Int, Int)