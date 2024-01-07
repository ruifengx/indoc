-- | Dynamic formatting: format string is only known until runtime.
module Data.String.Format.Dynamic
  ( FormatError (..)
  , ErrorKind (..)
  , Format
  , render
  , DynamicArgument (..)
  , Arguments (..)
  , renderFragment
  ) where

import Data.Foldable (fold)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Vector (Vector)
import Data.Vector qualified as V

import Data.String.Format
  ( FormatArgument (..)
  , FormatFragment (..)
  , FormatFunc
  , FormatTarget (..)
  , Parameter (..)
  , Selector (..)
  )
import Data.String.Format.Parser (Format)

-- | Runtime error for rendering a format string.
data FormatError s = FormatError
  -- | Selector for the first erroneous format argument.
  { selector  :: Selector s
  -- | Detailed error message.
  , errorKind :: ErrorKind s
  } deriving (Show, Eq)

-- | Error kind for rendering a format string.
data ErrorKind s
  -- | A required format argument is not provided.
  = Missing
  -- | A format argument does not support to be used as length.
  | CannotUseAsLength
  -- | A format argument does not support to be rendered as the specified kind.
  | UnsupportedKind s
  deriving (Show, Eq)

-- | Dynamic format argument.
data DynamicArgument s
  = forall a. DynamicArgument
  -- | Erased raw value of the argument.
  { rawValue        :: !a
  -- | Extract the raw value as length.
  , extractCount    :: Maybe Int
  -- | Format the raw value using provided style, keyed with format kind.
  , formatFunctions :: HashMap s (FormatFunc s a)
  }

-- | Collected format arguments.
data Arguments s = Arguments
  -- | Positional format arguments, indexed with natural numbers.
  { positional :: Vector (DynamicArgument s)
  -- | Named format arguments, indexed with their names.
  , named      :: HashMap s (DynamicArgument s)
  }

-- | Render the parsed format string with given arguments.
render :: (FormatTarget s, Hashable s) => Format s -> Arguments s -> Either (FormatError s) (Target s)
render fmt args = fold <$> traverse (renderFragment args) fmt

-- | Render a single format fragment with given arguments.
renderFragment :: (FormatTarget s, Hashable s) => Arguments s -> FormatFragment s -> Either (FormatError s) (Target s)
renderFragment _    (TextFragment text) = Right (plain text)
renderFragment args (ArgFragment fmt) = do
  arg <- resolveSelector args fmt.selector
  style <- traverse (resolveParameter args) fmt.style
  renderArgument arg (fmt { style })

renderArgument :: Hashable s => DynamicArgument s -> FormatArgument s Int -> Either (FormatError s) (Target s)
renderArgument DynamicArgument { rawValue, formatFunctions } fmt =
  maybeToEither (FormatError fmt.selector (UnsupportedKind fmt.kind))
    ((\func -> func fmt.style rawValue) <$> formatFunctions M.!? fmt.kind)

resolveSelector :: Hashable s => Arguments s -> Selector s -> Either (FormatError s) (DynamicArgument s)
resolveSelector args s = maybeToEither (FormatError s Missing) (fetch s)
  where fetch (Index n) = args.positional V.!? n
        fetch (Name n)  = args.named M.!? n

resolveParameter :: Hashable s => Arguments s -> Parameter s -> Either (FormatError s) Int
resolveParameter _    (Constant k) = Right k
resolveParameter args (Selector s) = do
  arg <- resolveSelector args s
  maybeToEither (FormatError s CannotUseAsLength) arg.extractCount

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right
