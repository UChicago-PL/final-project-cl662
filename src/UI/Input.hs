-- src/UI/Input.hs
module UI.Input
  ( promptPos
  , parsePos
  ) where

import Data.Char (isSpace)

import Game.Types (Pos)

promptPos :: String -> IO Pos
promptPos msg = do
  putStr msg
  s <- getLine
  case parsePos s of
    Right p -> pure p
    Left err -> do
      putStrLn ("Invalid input: " ++ err)
      putStrLn "Examples: 1,2    1 2    (1,2)"
      promptPos msg

-- | Parse a position from user input.
-- Accepted forms: "r,c", "r c", "(r,c)", "(r, c)".
parsePos :: String -> Either String Pos
parsePos raw =
  let s = trim raw
  in case s of
      "" -> Left "empty input"
      _  ->
        let s' = stripParens s
            s'' = map (\ch -> if ch == ',' then ' ' else ch) s'
            ws = words s''
        in case ws of
            [a,b] ->
              case (readInt a, readInt b) of
                (Just r, Just c) -> Right (r,c)
                _ -> Left "could not parse integers"
            _ -> Left "expected two integers"

stripParens :: String -> String
stripParens s =
  case s of
    ('(' : rest) | not (null rest) && last rest == ')' -> init rest
    _ -> s

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

readInt :: String -> Maybe Int
readInt t =
  case reads t of
    [(n, "")] -> Just n
    _         -> Nothing