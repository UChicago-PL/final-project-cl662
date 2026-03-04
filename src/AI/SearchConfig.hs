module AI.SearchConfig
  ( SearchConfig(..)
  , defaultConfig
  ) where

data SearchConfig = SearchConfig
  { maxDepth :: Int
  } deriving (Eq, Show)

defaultConfig :: SearchConfig
defaultConfig = SearchConfig { maxDepth = 3 }