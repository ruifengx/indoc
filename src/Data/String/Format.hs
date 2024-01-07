-- | Modern formatting interface, generic in the string type.
module Data.String.Format
  ( FormatStyle (..)
  , defaultFormatStyle
  , Alignment (..)
  , Parameter (..)
  , FormatTarget (..)
  , FormatFunc
  , FormatFragment (..)
  , FormatArgument (..)
  , Selector (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TB

-- | Format style information extracted from the format string.
data FormatStyle p = FormatStyle
  -- | Alignment: left, right, centre, or none.
  { alignment :: !(Maybe Alignment)
  -- | Character to fill for alignment.
  , fillChar  :: !(Maybe Char)
  -- | Whether the @+@ flag is specified (asking for explicit positive sign).
  , signPlus  :: !Bool
  -- | Whether the @-@ flag is specified.
  , signMinus :: !Bool
  -- | Whether the @#@ flag is specified (asking for alternate style).
  , alternate :: !Bool
  -- | Whether the @0@ flag is specified (asking for sign aware zero left-padding).
  , signZero  :: !Bool
  -- | Expected width for the output.
  , width     :: !(Maybe p)
  -- | Precision for numeric types, or maximum width for string types.
  , precision :: !(Maybe p)
  } deriving stock (Show, Eq, Functor, Foldable, Traversable)

-- | Default format style, corresponding to empty format argument.
defaultFormatStyle :: FormatStyle p
defaultFormatStyle = FormatStyle
  { alignment = Nothing
  , fillChar  = Nothing
  , signPlus  = False
  , signMinus = False
  , alternate = False
  , signZero  = False
  , width     = Nothing
  , precision = Nothing
  }

-- | Format alignment.
data Alignment
  = ALeft    -- ^ Left alignment (@<@).
  | ACentre  -- ^ Centre alignment (@^@).
  | ARight   -- ^ Right alignment (@>@).
  deriving stock (Show, Eq)

-- | Width and precision parameter.
data Parameter s
  -- | Constant parameter.
  = Constant !Int
  -- | Parameter from format argument.
  | Selector (Selector s)
  deriving stock (Show, Eq)

-- | Format function from @a@ to format output @s@.
type FormatFunc s a = FormatStyle Int -> a -> Target s

-- | Format target string type.
class Monoid (Target s) => FormatTarget s where
  -- | Target string type, notably 'ShowS' for 'String' and 'Builder' for 'Text'.
  type Target s :: Type
  -- | Embed a string fragment into the target string type.
  plain :: s -> Target s

instance FormatTarget String where
  type Target String = ShowS
  plain = showString

instance FormatTarget Text where
  type Target Text = Builder
  plain = TB.fromText

-- | Fragment of a format string, parametric in the format string type.
data FormatFragment s
  -- | Pure text without format instructions.
  = TextFragment s
  -- | Format argument.
  | ArgFragment (FormatArgument s (Parameter s))
  deriving stock (Show, Eq)

-- | Format argument.
data FormatArgument s p
  = FormatArgument
  -- | Select the argument as contents for formatting.
  { selector :: Selector s
  -- | Format style for the selected argument.
  , style    :: FormatStyle p
  -- | Format type, how to format the selected argument.
  , kind     :: s
  } deriving stock (Show, Eq)

-- | Selector for format arguments.
data Selector s
  = Index Int -- ^ Select by argument index.
  | Name s    -- ^ Select by argument name.
  deriving stock (Show, Eq)
