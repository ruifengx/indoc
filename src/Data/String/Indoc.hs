-- | Compile time unindent with Template Haskell.
module Data.String.Indoc (indoc) where

import Data.String.Unindent (unindent)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL), Pat (LitP), Q)

-- | Unindent and produce a string literal as an expression or pattern.
indoc :: QuasiQuoter
indoc = QuasiQuoter
  { quoteExp = pure . LitE . StringL . unindent
  , quotePat = pure . LitP . StringL . unindent
  , quoteDec = unsupported "declaration"
  , quoteType = unsupported "type"
  }

unsupported :: String -> a -> Q b
unsupported kind _ = fail ("indoc cannot be used to generate a " ++ kind ++ ".")
