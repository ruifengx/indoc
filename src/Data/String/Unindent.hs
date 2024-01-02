-- | Runtime unindent without Template Haskell.
module Data.String.Unindent (unindent) where

import Data.Char (isSpace)
import Data.Function ((&))

-- | Unindent the paragraph.
--   Remove the same number of spaces, as many as possible, from each line.
unindent :: String -> String
unindent s = concatMap trans theLines
  where
    trans l = if all isSpace l then "\n" else drop indentLevel l
    indentLevel = theLines
      & filter (not . all isSpace)
      & map (length . takeWhile isSpace)
      & minimum
    theLines = dropEmpty (breakLines (filter (/= '\r') s))
    dropEmpty (x : xs) | all isSpace x = xs
    dropEmpty xs = xs

breakLines :: String -> [String]
breakLines s
  | lfFound || nonEmpty = firstLine : breakLines rest
  | otherwise = []
  where (lfFound, firstLine, rest) = breakAfter (== '\n') s
        nonEmpty = not (all isSpace firstLine)

breakAfter :: (a -> Bool) -> [a] -> (Bool, [a], [a])
breakAfter p = go
  where go [] = (False, [], [])
        go (x : xs)
          | p x = (True, [x], xs)
          | otherwise = (found, x : ys, zs)
          where (found, ys, zs) = go xs
