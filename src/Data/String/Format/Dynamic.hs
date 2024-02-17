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
  (FormatArgument (..), FormatFragment (..), Parameter (..), RawFormatFunc, Selector (..))
import Data.String.Format.Parser (Format)
import Data.String.Format.StringBuilder (Builder, StringBuilder (BuildOutput, embed))

-- | Runtime error for rendering a format string.
data FormatError s = FormatError
  -- | Selector for the first erroneous format argument.
  { selector  :: Selector s
  -- | Detailed error message.
  , errorKind :: ErrorKind s
  } deriving stock (Show, Eq)

-- | Error kind for rendering a format string.
data ErrorKind s
  -- | A required format argument is not provided.
  = Missing
  -- | A format argument does not support to be used as length.
  | CannotUseAsLength
  -- | A format argument does not support to be rendered as the specified kind.
  | UnsupportedKind s
  deriving stock (Show, Eq)

-- | Dynamic format argument.
data DynamicArgument s b
  = forall a. DynamicArgument
  -- | Erased raw value of the argument.
  { rawValue        :: !a
  -- | Extract the raw value as length.
  , extractCount    :: Maybe Int
  -- | Format the raw value using provided style, keyed with format kind.
  , formatFunctions :: HashMap s (RawFormatFunc b a)
  }

-- | Collected format arguments.
data Arguments s b = Arguments
  -- | Positional format arguments, indexed with natural numbers.
  { positional :: Vector (DynamicArgument s b)
  -- | Named format arguments, indexed with their names.
  , named      :: HashMap s (DynamicArgument s b)
  }

-- | Render the parsed format string with given arguments.
render :: (StringBuilder b, Hashable s, s ~ BuildOutput b)
  => Format s
  -> Arguments s b
  -> Either (FormatError s) (Builder b)
render fmt args = fold <$> traverse (renderFragment args) fmt

-- | Render a single format fragment with given arguments.
renderFragment :: (StringBuilder b, Hashable s, s ~ BuildOutput b)
  => Arguments s b
  -> FormatFragment s
  -> Either (FormatError s) (Builder b)
renderFragment _    (TextFragment text) = Right (embed text)
renderFragment args (ArgFragment fmt) = do
  arg <- resolveSelector args fmt.selector
  style <- traverse (resolveParameter args) fmt.style
  renderArgument arg (fmt { style })

renderArgument :: Hashable s
  => DynamicArgument s b
  -> FormatArgument s Int
  -> Either (FormatError s) (Builder b)
renderArgument DynamicArgument { rawValue, formatFunctions } fmt =
  maybeToEither (FormatError fmt.selector (UnsupportedKind fmt.kind))
    ((\func -> func fmt.style rawValue) <$> formatFunctions M.!? fmt.kind)

resolveSelector :: Hashable s => Arguments s b -> Selector s -> Either (FormatError s) (DynamicArgument s b)
resolveSelector args s = maybeToEither (FormatError s Missing) (fetch s)
  where fetch (Index n) = args.positional V.!? n
        fetch (Name n)  = args.named M.!? n

resolveParameter :: Hashable s => Arguments s b -> Parameter s -> Either (FormatError s) Int
resolveParameter _    (Constant k) = Right k
resolveParameter args (Selector s) = do
  arg <- resolveSelector args s
  maybeToEither (FormatError s CannotUseAsLength) arg.extractCount

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right
