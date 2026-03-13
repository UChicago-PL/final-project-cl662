-- src/AI/SearchConfig.hs
module AI.SearchConfig
  ( SearchConfig(..)
  , defaultConfig
  ) where

data SearchConfig = SearchConfig
  { maxDepth          :: Int
  , useAlphaBeta      :: Bool
  , useMoveOrdering   :: Bool
  , nodeLimit         :: Maybe Int
  , neighborRadius    :: Int
  }

defaultConfig :: SearchConfig
defaultConfig =
  SearchConfig
    { maxDepth = 7
    , useAlphaBeta = True
    , useMoveOrdering = True
    , nodeLimit = Just 25000
    , neighborRadius    = 1
    }