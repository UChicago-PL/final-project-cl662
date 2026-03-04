module UI.Loop
  ( run
  ) where

import Game.State (initState)
import UI.Ansi (clearScreenSafe)
import UI.Render (render)

run :: IO ()
run = do
  let st = initState 3 3 3
  clearScreenSafe
  putStrLn "mnk-game starting (stub)..."
  putStrLn (render st)
  putStrLn "Done."