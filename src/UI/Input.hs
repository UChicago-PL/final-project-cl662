-- src/UI/Input.hss
module UI.Input
  ( promptPos
  , parsePos
  , promptMNK
  , parseMNK
  ) where

import Data.Char (isSpace)

import Game.Types (Pos)


-- Repeatedly asks the user for a board position until they enter a valid one.
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

--This function prompts for an mnk input for game type
promptMNK :: (Int, Int, Int) -> IO (Int, Int, Int)
promptMNK def = do
  let (dm, dn, dk) = def
  putStrLn "Enter m n k (rows cols k). Press Enter for default."
  putStrLn ("Default: " ++ show dm ++ " " ++ show dn ++ " " ++ show dk)
  s <- getLine
  let t = trim s
  if t == ""
    then pure def
    else case parseMNK t of
      Right mnk -> pure mnk
      Left err -> do
        putStrLn ("Invalid m n k: " ++ err)
        putStrLn "Examples: 3 3 3   |   6 7 4"
        promptMNK def

--parser helper
parseMNK :: String -> Either String (Int, Int, Int)
parseMNK raw =
  let s = trim raw
      ws = words s
  in case ws of
      [a,b,c] ->
        case (readInt a, readInt b, readInt c) of
          (Just m, Just n, Just k)
            | m <= 0 || n <= 0 -> Left "m and n must be positive"
            | k <= 0           -> Left "k must be positive"
            | k > max m n      -> Left "k cannot exceed max(m,n) (no possible win lines)"
            | otherwise        -> Right (m,n,k)
          _ -> Left "could not parse integers"
      _ -> Left "expected three integers"


-- Removes one pair of surrounding parentheses from a string if they are present.
stripParens :: String -> String
stripParens s =
  case s of
    ('(' : rest) | not (null rest) && last rest == ')' -> init rest
    _ -> s

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


--Removes trailing elements from a list as long as they satisfy a given condition.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

readInt :: String -> Maybe Int
readInt t =
  case reads t of
    [(n, "")] -> Just n
    _         -> Nothing