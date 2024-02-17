{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Format type classes for polymorphic text formatting.
module Data.String.Format.Class
  ( Debug (fmtDebug)
  , Display (fmtDisplay)
  , Binary (fmtBinary)
  , Octal (fmtOctal)
  , LowerHex (fmtLowerHex)
  , UpperHex (fmtUpperHex)
  , Pointer (fmtPointer)
  , LowerExp (fmtLowerExp)
  , UpperExp (fmtUpperExp)
  , FmtShow (FmtShow)
  , fmtAlign
  ) where

import Data.Char (intToDigit, toUpper)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (fromMaybe)
import Data.Ratio (Ratio, denominator, numerator)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CInt, CIntMax, CIntPtr, CPtrdiff, CSize, CUInt, CUIntMax, CUIntPtr)
import Foreign.Ptr (IntPtr, Ptr, WordPtr, ptrToWordPtr)
import GHC.Generics
  ( C
  , Constructor (conIsRecord, conName)
  , D
  , Generic (Rep, from)
  , Generically (Generically)
  , K1 (K1)
  , M1 (M1)
  , S
  , Selector (selName)
  , U1 (U1)
  , V1
  , type (:*:) ((:*:))
  , type (:+:) (L1, R1)
  )
import Language.Haskell.TH.Lib (conT, varE, varP, varT)
import Language.Haskell.TH.Syntax (Dec (InstanceD), mkName)
import Numeric (showIntAtBase)
import Numeric.Natural (Natural)

import Data.String.Format (Alignment (..), FormatFunc, FormatStyle (..))
import Data.String.Format.StringBuilder (Builder, StringBuilder (..), indent, newline)

-- | Debug format: @?@, can be derived via 'Generically'.
class Debug a where
  -- | Format the argument using the debug format.
  fmtDebug :: FormatFunc b a

-- | Display format.
class Display a where
  -- | Format the argument using the display format.
  fmtDisplay :: FormatFunc b a

-- | Binary format: @b@, accepts @#@ for a @0b@ prefix.
class Binary a where
  -- | Format the argument as binary numbers.
  fmtBinary :: FormatFunc b a

-- | Octal format: @o@, accepts @#@ for a @0o@ prefix.
class Octal a where
  -- | Format the argument as octal numbers.
  fmtOctal :: FormatFunc b a

-- | Lower hexadecimal format: @x@, accepts @#@ for a @0x@ prefix.
class LowerHex a where
  -- | Format the argument as lowercase hexadecimal numbers.
  fmtLowerHex :: FormatFunc b a

-- | Upper hexadecimal format: @X@, accepts @#@ for a @0x@ prefix.
class UpperHex a where
  -- | Format the argument as uppercase hexadecimal numbers.
  fmtUpperHex :: FormatFunc b a

-- | Pointer format: @p@, address as hexadecimal integers.
class Pointer a where
  -- | Format the argument as an address.
  fmtPointer :: FormatFunc b a

-- | Scientific format with lowercase @e@: @e@.
class LowerExp a where
  -- | Format the argument using scientific notation with lowercase @e@.
  fmtLowerExp :: FormatFunc b a

-- | Scientific format with uppercase @E@: @E@.
class UpperExp a where
  -- | Format the argument using scientific notation with uppercase @E@.
  fmtUpperExp :: FormatFunc b a

-- | Wrapper for using 'Show' to format an argument.
newtype FmtShow a = FmtShow a

instance Show a => Debug (FmtShow a) where
  fmtDebug style (FmtShow x) = fmtAlign ALeft (length s) style (fromString s)
    where s = show x

instance Show a => Display (FmtShow a) where
  fmtDisplay = fmtDebug

-- | Handle alignment for the format argument.
--
-- - Skip if 'Data.String.Format.signZero' is specified.
-- - Skip if actual width is wider than expected width.
-- - Fill with space (@' '@) by default if 'Data.String.Format.fillChar' is 'Nothing'.
-- - Use the specified default alignment if 'Data.String.Format.alignment' is 'Nothing'.
fmtAlign :: Alignment -> Int -> FormatFunc b (Builder b)
fmtAlign defaultAlign actualWidth style s
  | Just width <- style.width
  , not style.signZero
  , actualWidth < width
  = let alignment = fromMaybe defaultAlign style.alignment
        fillChar = fromMaybe ' ' style.fillChar
        totalFill = width - actualWidth
        (leftFill, rightFill) = case alignment of
          ALeft   -> (0, totalFill)
          ARight  -> (totalFill, 0)
          ACentre -> let l = totalFill `quot` 2 in (l, totalFill - l)
        fillStr n = fromString (replicate n fillChar)
    in fillStr leftFill <> s <> fillStr rightFill
  | otherwise = s

fmtIntegral :: (Integral a, StringBuilder b) => a -> String -> Bool -> FormatFunc b a
fmtIntegral base prefix upper style n = fmtAlign ARight (length full) style (fromString full)
  where full = sign <> truePrefix <> replicate nZero '0' <> body
        toDigit = if upper then toUpper . intToDigit else intToDigit
        body = showIntAtBase base toDigit n ""
        sign | style.signPlus, n > 0 = "+"
             | n < 0 = "-"
             | otherwise = ""
        truePrefix = if style.alternate then prefix else ""
        nOthers = length sign + length body + length truePrefix
        nZero = maybe 0 (subtract nOthers) style.width
{-# INLINE fmtIntegral #-}

fmtRatio :: FormatFunc b n -> FormatFunc b (Ratio n)
fmtRatio fmt style q = fmt style n <> fromString slash <> fmt style d
  where n = numerator q; d = denominator q
        slash = if style.alternate then " / " else "/"
{-# INLINE fmtRatio #-}

let
  intTypes =
    [ ''Int, ''Int8, ''Int16, ''Int32, ''Int64
    , ''Word, ''Word8, ''Word16, ''Word32, ''Word64
    , ''CInt, ''CIntPtr, ''CIntMax
    , ''CUInt, ''CUIntPtr, ''CUIntMax
    , ''CSize, ''CPtrdiff
    , ''Integer, ''Natural
    , ''IntPtr, ''WordPtr ]
  intClasses =
    [ (''Debug,    'fmtDebug,    10,  "",   False)
    , (''Display,  'fmtDisplay,  10,  "",   False)
    , (''Binary,   'fmtBinary,   2,   "0b", False)
    , (''Octal,    'fmtOctal,    8,   "0o", False)
    , (''LowerHex, 'fmtLowerHex, 16,  "0x", False)
    , (''UpperHex, 'fmtUpperHex, 16,  "0x", True) ]
  intFmt (ty, (cls, func, base :: Int, prefix, upper)) = do
    let header = [t| $(conT cls) $(conT ty) |]
    let funcDef = [d| $(varP func) = fmtIntegral base prefix upper |]
    InstanceD Nothing [] <$> header <*> funcDef
  ratioFmt (cls, func, _, _, _) = do
    let n = mkName "n"
    ctxt <- [t| $(conT cls) $(varT n) |]
    let header = [t| $(conT cls) (Ratio $(varT n)) |]
    let funcDef = [d| $(varP func) = fmtRatio $(varE func) |]
    InstanceD Nothing [ctxt] <$> header <*> funcDef
  in traverse intFmt (liftA2 (,) intTypes intClasses)
  <> traverse ratioFmt intClasses

instance Pointer (Ptr a) where
  fmtPointer style p = fmtLowerHex pointerStyle (ptrToWordPtr p)
    where pointerStyle = style { alternate = True }

-- | 'Debug' implementation for 'Generic' data types.
class GDebug f where
  gFmtDebug :: FormatFunc b (f p)

-- Rust-style formatting: 'C(x,y,z)' instead of 'C x y z' for tuple constructors
instance (Generic a, GDebug (Rep a)) => Debug (Generically a) where
  fmtDebug style (Generically a) = gFmtDebug style (from a)

class IsUnitary f where
  isUnitary :: f p -> Bool

instance IsUnitary V1 where
  isUnitary v1 = case v1 of

instance IsUnitary U1 where
  isUnitary _ = True

instance IsUnitary (f :*: g) where
  isUnitary _ = False

instance GDebug V1 where
  gFmtDebug _ v1 = case v1 of

instance GDebug U1 where
  gFmtDebug _ U1 = mempty

instance Debug c => GDebug (K1 i c) where
  gFmtDebug style (K1 x) = fmtDebug style x

instance (GDebug f, GDebug g) => GDebug (f :*: g) where
  gFmtDebug style (x :*: y) = gFmtDebug style x <> fromString "," <> sep <> gFmtDebug style y
    where sep = if not style.alternate then mempty else newline

instance (GDebug f, GDebug g) => GDebug (f :+: g) where
  gFmtDebug style (L1 x) = gFmtDebug style x
  gFmtDebug style (R1 y) = gFmtDebug style y

instance GDebug f => GDebug (M1 D d f) where
  gFmtDebug style (M1 d) = gFmtDebug style d

instance (Constructor c, IsUnitary f, GDebug f) => GDebug (M1 C c f) where
  gFmtDebug style m@(M1 c) = fromString (conName m) <> brace
    where brace | isUnitary c = rawBody
                | conIsRecord m = lBrace <> body <> rDelim "}"
                | otherwise = fromString "(" <> body <> rDelim ")"
          alt x y = if not style.alternate then x else y
          rawBody = gFmtDebug style c
          body = alt rawBody (indent 2 (newline <> rawBody))
          lBrace = alt (fromString "{") (fromString " {")
          rDelim sym = alt (fromString sym) (newline <> fromString sym)

instance (Selector s, GDebug f) => GDebug (M1 S s f) where
  gFmtDebug style m@(M1 s) = prefix (selName m) <> gFmtDebug style s
    where prefix name = if null name then mempty else fromString name <> eq
          eq = if not style.alternate then fromString "=" else fromString " = "
