-- | Modern formatting interface, generic in the string type.
module Data.String.Format
  ( FormatStyle (..)
  , defaultFormatStyle
  , Alignment (..)
  , FormatFunc
  , FormatFragment (..)
  , FormatArgument (..)
  , Selector (..)
  ) where

-- | Format style information extracted from the format string.
data FormatStyle = FormatStyle
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
  , width     :: !(Maybe Int)
  -- | Precision for numeric types, or maximum width for string types.
  , precision :: !(Maybe Int)
  } deriving (Show, Eq)

-- | Default format style, corresponding to empty format argument.
defaultFormatStyle :: FormatStyle
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
  deriving (Show, Eq)

-- | Format function from @a@ to format output @s@.
type FormatFunc s a = FormatStyle -> a -> s

-- | Fragment of a format string, parametric in the format string type.
data FormatFragment s
  -- | Pure text without format instructions.
  = TextFragment s
  -- | Format argument.
  | ArgFragment (FormatArgument s)
  deriving (Show, Eq)

-- | Format argument.
data FormatArgument s
  = FormatArgument
  -- | Select the argument as contents for formatting.
  { selector :: Selector s
  -- | Format style for the selected argument.
  , style    :: FormatStyle
  -- | Format type, how to format the selected argument.
  , kind     :: s
  } deriving (Show, Eq)

-- | Selector for format arguments.
data Selector s
  = Index Int -- ^ Select by argument index.
  | Name s    -- ^ Select by argument name.
  deriving (Show, Eq)
