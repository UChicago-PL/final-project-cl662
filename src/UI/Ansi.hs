module UI.Ansi
  ( clearScreenSafe
  ) where

import System.Console.ANSI (clearScreen, setCursorPosition)

clearScreenSafe :: IO ()
clearScreenSafe = do
  clearScreen
  setCursorPosition 0 0