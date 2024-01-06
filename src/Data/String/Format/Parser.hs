-- | Parser for format strings.
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE RecordWildCards #-}
module Data.String.Format.Parser
  ( Source (..)
  , parseFormat
  , parseFormatArgument
  ) where

import Prelude hiding (length, null, span)

import Control.Applicative (Alternative (some, (<|>)), optional)
import Control.Applicative qualified as A
import Control.Monad (unless)
import Control.Monad.Except (Except, ExceptT (ExceptT), MonadError (throwError))
import Control.Monad.State.Strict (MonadState (get, put, state), StateT (StateT), gets)
import Data.Bifunctor (bimap)
import Data.Char (digitToInt, isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Foldable (traverse_)
import Data.Functor (void, ($>))
import Data.Functor.Identity (Identity (Identity))
import Data.List qualified as L
import Data.List.NonEmpty qualified as NL
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T

import Data.String.Format
  ( Alignment (..)
  , FormatArgument (..)
  , FormatFragment (..)
  , FormatStyle (..)
  , Parameter (..)
  , Selector (..)
  , defaultFormatStyle
  )

-- | Format string types.
class Source s where
  -- | Check if the string is empty.
  null :: s -> Bool
  -- | Break the string into a longest prefix satisfying the predicate and the remaining suffix.
  span :: (Char -> Bool) -> s -> (s, s)
  -- | Break the string into the first character and the remaining suffix.
  uncons :: s -> Maybe (Char, s)
  -- | Length of this string.
  length :: s -> Int

  -- | Empty string.
  empty :: s
  -- | Construct a string from a 'String'.
  fromString :: String -> s
  -- | Get a 'String' for error message.
  toString :: s -> String

instance Source String where
  null = L.null
  span = L.span
  uncons = L.uncons
  length = L.length
  empty = ""
  fromString = id
  toString = id

instance Source Text where
  null = T.null
  span = T.span
  uncons = T.uncons
  length = T.length
  empty = T.empty
  fromString = T.pack
  toString = T.unpack

-- | Parse a full format string.
parseFormat :: Source s => s -> Either String [FormatFragment s]
parseFormat s = bimap errorString fst $ runP full (PState s 0 0)

-- | Parse a single format argument.
parseFormatArgument :: Source s => s -> Either String (FormatArgument s)
parseFormatArgument s = bimap errorString fst $ runP formatArgument (PState s 0 0)

data PState s = PState
  { inputSource :: !s
  , _argIndex   :: !Int
  , byteOffset  :: !Int
  }

data Error = Error
  { position :: !Int
  , messages :: [Message]
  } deriving (Show, Eq)

splitMessage :: [Message] -> ([String], [String])
splitMessage [] = ([], [])
splitMessage (x : xs)
  | Expected y <- x   = (y : ys, zs)
  | Unexpected z <- x = (ys, z : zs)
  where (ys, zs) = splitMessage xs

errorString :: Error -> String
errorString (Error n msgs) = prompt <> message
  where (expected, unexpected) = bimap joinMsg joinMsg (splitMessage msgs)
        uniq = map NL.head . NL.group . L.sort
        joinMsg = L.intercalate " or " . uniq
        prompt = "at offset " <> show n <> ": "
        message
          | L.null expected, L.null unexpected = "unknown error"
          | L.null expected = "unexpected " <> unexpected
          | L.null unexpected = "expecting " <> expected
          | otherwise = "expecting " <> expected <> ", found " <> unexpected

instance Semigroup Error where
  e1@(Error pos1 msgs1) <> e2@(Error pos2 msgs2)
    | L.null msgs1, not (L.null msgs2) = e2
    | L.null msgs2, not (L.null msgs1) = e1
    | otherwise = case compare pos1 pos2 of
      LT -> e2
      GT -> e1
      EQ -> Error pos1 (msgs1 ++ msgs2)

data Message
  = Unexpected String
  | Expected String
  deriving (Show, Eq, Ord)

newtype P s a = P { runP :: PState s -> Either Error (a, PState s) }
  deriving (Functor, Applicative, Monad, MonadState (PState s), MonadError Error)
    via StateT (PState s) (Except Error)

instance Alternative (P s) where
  empty = throw []
  P f <|> P g = P $ \st -> case (f st, g st) of
    (Right a, _)     -> Right a
    (_, Right b)     -> Right b
    (Left l, Left r) -> Left (l <> r)

throw :: [Message] -> P s a
throw msgs = do
  n <- gets byteOffset
  throwError (Error n msgs)

lookahead :: P s a -> P s ()
lookahead p = do
  beforeState <- get
  void p
  put beforeState

eof :: Source s => String -> P s ()
eof name = do
  s <- gets inputSource
  unless (null s) (throw [Unexpected ("extra text in " <> name <> ": " <> toString s)])

scoped :: P s t -> P t a -> P s a
scoped scope p = do
  offset0 <- gets byteOffset
  t <- scope
  PState s n offset <- get
  (a, PState _ n' _) <- case runP p (PState t n offset0) of
    Right r  -> pure r
    Left err -> throwError err
  put (PState s n' offset)
  pure a

nextIndex :: P s (Selector s)
nextIndex = state (\(PState s n k) -> (Index n, PState s (succ n) k))

munch :: Source s => (Char -> Bool) -> P s s
munch p = state (\(PState s n k) -> let (t, s') = span p s in (t, PState s' n (k + length t)))

satisfy :: Source s => String -> (Char -> Bool) -> P s Char
satisfy expected p = P $ \(PState s n k) -> case uncons s of
  Just (x, s')
    | p x -> Right (x, PState s' n (succ k))
    | otherwise -> Left $ Error k [Expected expected, Unexpected ("character '" ++ [x] ++ "'")]
  Nothing -> Left $ Error k [Expected expected, Unexpected "end of string"]

anyChar :: Source s => String -> P s Char
anyChar expected = satisfy expected (const True)

char :: Source s => Char -> P s ()
char c = void (satisfy ['\'', c, '\''] (== c))

string :: Source s => String -> P s ()
string = traverse_ char

normal :: Source s => P s s
normal = munch (`notElem` "{}")

escaped :: Source s => P s (FormatFragment s)
escaped =
      string "{{" $> TextFragment (fromString "{")
  <|> string "}}" $> TextFragment (fromString "}")
  <|> char '{' *> scoped normal (ArgFragment <$> formatArgument) <* char '}'

integer :: Source s => P s Int
integer = foldl (\acc d -> acc * 10 + d) 0 <$> some digit
  where digit = digitToInt <$> satisfy "digit" isDigit

identifier :: Source s => P s s
identifier = lookahead (satisfy "identifier" isIdStart) *> munch isIdChar
  where isIdChar c = isAlphaNum c || c == '_'
        isIdStart c = isAlpha c || c == '_'

count :: Source s => P s (Parameter s)
count = Selector <$> argument <* char '$' <|> Constant <$> integer

argument :: Source s => P s (Selector s)
argument = Name <$> identifier <|> Index <$> integer

formatArgument :: Source s => P s (FormatArgument s)
formatArgument = do
  selector <- argument <|> nextIndex
  spec <- optional $ do
    char ':'
    let align = char '<' $> ALeft <|> char '^' $> ACentre <|> char '>' $> ARight
    let fullAlign = (,) <$> anyChar "fill character" <*> align
    (fillChar, alignment) <- bimap Just Just <$> fullAlign <|> (Nothing, ) <$> optional align
    (signPlus, signMinus) <-
          char '+' $> (True, False)
      <|> char '-' $> (False, True)
      <|> pure (False, False)
    alternate <- isJust <$> optional (char '#')
    signZero <- isJust <$> optional (char '0')
    width <- optional count
    precision <- optional (char '.' *> (count <|> char '*' *> (Selector <$> nextIndex)))
    kind <- string "?" $> fromString "?" <|> identifier <|> pure empty
    pure (FormatStyle {..}, kind)
  -- Rust allows trailing space, let's also allow it
  -- it is probably for aligning the format string itself
  void (munch isSpace)
  eof "format argument"
  let (style, kind) = fromMaybe (defaultFormatStyle, empty) spec
  pure FormatArgument { selector, style, kind }

full :: Source s => P s [FormatFragment s]
full = do
  leadingText <- normal
  let t = if null leadingText then id else (TextFragment leadingText :)
  rest <- (:) <$> escaped <*> (full <|> pure []) <|> pure []
  pure (t rest)
