module Data.String.Format.StringBuilder
  ( StringBuilder (..)
  , Builder
  , indent
  , newline
  ) where

import Data.Kind (Type)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB

class Monoid b => StringBuilder b where
  type BuildOutput b :: Type
  embed :: BuildOutput b -> b
  fromString :: String -> b
  fromShowS :: ShowS -> b
  build :: b -> BuildOutput b

instance StringBuilder ShowS where
  type BuildOutput ShowS = String
  embed = showString
  fromString = showString
  fromShowS = id
  build b = b ""

instance StringBuilder TB.Builder where
  type BuildOutput TB.Builder = T.Text
  embed = TB.fromText
  fromString = TB.fromText . T.pack
  fromShowS f = fromString (f "")
  build = TL.toStrict . TB.toLazyText

newtype Builder b = Builder (Int -> b)
  deriving newtype (Semigroup, Monoid)

indent :: Int -> Builder b -> Builder b
indent n (Builder b) = Builder (b . (+ n))

newline :: StringBuilder b => Builder b
newline = Builder (\n -> fromString ('\n' : replicate n ' '))

instance StringBuilder b => StringBuilder (Builder b) where
  type BuildOutput (Builder b) = BuildOutput b
  embed = Builder . const . embed
  fromString = Builder . const . fromString
  fromShowS = Builder . const . fromShowS
  build (Builder b) = build (b 0)
