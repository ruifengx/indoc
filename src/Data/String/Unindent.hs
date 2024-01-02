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
    theLines = dropEmpty (breakLines s)
    dropEmpty (x : xs) | all isSpace x = xs
    dropEmpty xs = xs

breakLines :: String -> [String]
breakLines "" = []
breakLines s = firstLine : breakLines rest
  where (firstLine, rest) = breakAfter (== '\n') s

breakAfter :: (a -> Bool) -> [a] -> ([a], [a])
breakAfter p = go
  where go [] = ([], [])
        go (x : xs)
          | p x = ([x], xs)
          | otherwise = let (ys, zs) = go xs in (x : ys, zs)
