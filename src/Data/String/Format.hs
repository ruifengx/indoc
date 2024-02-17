-- | Modern formatting interface, generic in the string type.
module Data.String.Format
  ( FormatStyle (..)
  , defaultFormatStyle
  , Alignment (..)
  , Parameter (..)
  , FormatFunc
  , RawFormatFunc
  , FormatFragment (..)
  , FormatArgument (..)
  , Selector (..)
  ) where

import Data.String.Format.StringBuilder (Builder, StringBuilder)

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
type FormatFunc s a = StringBuilder s => RawFormatFunc s a

-- | Raw format function from @a@ to format output @s@.
type RawFormatFunc s a = FormatStyle Int -> a -> Builder s

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
