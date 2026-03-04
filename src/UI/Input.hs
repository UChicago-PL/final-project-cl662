module UI.Input
  ( promptLine
  ) where

promptLine :: String -> IO String
promptLine msg = do
  putStr msg
  getLine