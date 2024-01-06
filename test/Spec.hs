{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes    #-}

import Test.Hspec

import Data.String.Format
  ( Alignment (ARight)
  , FormatArgument (..)
  , FormatFragment (..)
  , FormatStyle (..)
  , Parameter (..)
  , Selector (..)
  , defaultFormatStyle
  )
import Data.String.Format.Parser (parseFormat, parseFormatArgument)
import Data.String.Indoc (indoc)
import Data.String.Unindent (unindent)

main :: IO ()
main = hspec do
  describe "unindent" do
    it "preserves the empty string" do
      unindent "" `shouldBe` ""
    it "preserves no line ending" do
      unindent "no line ending" `shouldBe` "no line ending"
    it "preserves a single line" do
      unindent "a single line\n" `shouldBe` "a single line\n"
    it "drops the first empty line" do
      unindent "\nit gets dropped" `shouldBe` "it gets dropped"
    it "keeps the second empty line" do
      unindent "\n\nthe second is kept" `shouldBe` "\nthe second is kept"
    it "keeps the trailing empty line" do
      unindent "this is kept\n" `shouldBe` "this is kept\n"
      unindent "this is kept\n  " `shouldBe` "this is kept\n"
    it "drops leading whitespace" do
      unindent "\n  indented\n    more indented\n  back"
        `shouldBe` "indented\n  more indented\nback"
      unindent "\n    more indented\n  indented"
        `shouldBe` "  more indented\nindented"
    it "replaces white lines with empty lines" do
      unindent "  a\n    \nb" `shouldBe` "  a\n\nb"
      unindent "  a\n    \n    b" `shouldBe` "a\n\n  b"
    it "ignores unindented empty lines" do
      unindent "  a\n\n    b" `shouldBe` "a\n\n  b"
  describe "indoc" do
    it "quotes a string literal expression" do
      let lhs = [indoc|
              some arbitrary
                indented
                  text
              with
              different
                levels
            |]
          rhs = unlines
            [ "some arbitrary"
            , "  indented"
            , "    text"
            , "with"
            , "different"
            , "  levels"
            ]
      lhs `shouldBe` rhs
    it "quotes a string literal pattern" do
      let p :: String -> Int
          p [indoc|xxx|] = 1
          p [indoc|yyy|] = 2
          p _            = 0
      p "xxx" `shouldBe` 1
      p "yyy" `shouldBe` 2
      p "zzz" `shouldBe` 0
  describe "parseFormat" do
    it "parses string without format arguments" do
      parseFormat "there is no argument" `shouldBe` Right [TextFragment "there is no argument"]
    it "parses string with escaped braces" do
      parseFormat "}} weird {{ combinations {{"
        `shouldBe` Right (map TextFragment ["}", " weird ", "{", " combinations ", "{"])
    it "parses string with format arguments" do
      parseFormat "some argument {} here" `shouldBe` Right
        [ TextFragment "some argument "
        , ArgFragment FormatArgument
          { selector = Index 0
          , style = defaultFormatStyle
          , kind = ""
          }
        , TextFragment " here"
        ]
    it "parses rich format strings" do
      parseFormat "xyz = {{{xyz:#?}}}, abc = {:+#0};" `shouldBe` Right
        [ TextFragment "xyz = "
        , TextFragment "{"
        , ArgFragment FormatArgument
          { selector = Name "xyz"
          , style = defaultFormatStyle { alternate = True }
          , kind = "?"
          }
        , TextFragment "}"
        , TextFragment ", abc = "
        , ArgFragment FormatArgument
          { selector = Index 0
          , style = defaultFormatStyle
            { alternate = True
            , signPlus = True
            , signZero = True
            }
          , kind = ""
          }
        , TextFragment ";"
        ]
  describe "parseFormatArgument" do
    it "parses empty argument" do
      parseFormatArgument "" `shouldBe` Right
        FormatArgument
        { selector = Index 0
        , style = defaultFormatStyle
        , kind = ""
        }
    it "parses alignment without fill character" do
      parseFormatArgument ":>x" `shouldBe` Right
        FormatArgument
        { selector = Index 0
        , style = defaultFormatStyle { alignment = Just ARight }
        , kind = "x"
        }
    it "parses alignment with fill character" do
      parseFormatArgument ":~>x" `shouldBe` Right
        FormatArgument
        { selector = Index 0
        , style = defaultFormatStyle { alignment = Just ARight, fillChar = Just '~' }
        , kind = "x"
        }
    it "parses plus or minus sign" do
      parseFormatArgument ":+p" `shouldBe` Right
        FormatArgument
        { selector = Index 0
        , style = defaultFormatStyle { signPlus = True }
        , kind = "p"
        }
      parseFormatArgument ":-p" `shouldBe` Right
        FormatArgument
        { selector = Index 0
        , style = defaultFormatStyle { signMinus = True }
        , kind = "p"
        }
    it "rejects both plus and minus sign" do
      parseFormatArgument ":+-x" `shouldBe` Left
        "at offset 2: unexpected extra text in format argument: -x"
    it "parses alternate sign" do
      parseFormatArgument ":#o" `shouldBe` Right
        FormatArgument
        { selector = Index 0
        , style = defaultFormatStyle { alternate = True }
        , kind = "o"
        }
    it "parses zero sign" do
      parseFormatArgument ":0X" `shouldBe` Right
        FormatArgument
        { selector = Index 0
        , style = defaultFormatStyle { signZero = True }
        , kind = "X"
        }
    describe "argument width" do
      let arg w = FormatArgument
            { selector = Index 0
            , style = defaultFormatStyle { width = Just w }
            , kind = "x"
            }
      it "parses constant argument width" do
        parseFormatArgument ":10x" `shouldBe` Right (arg (Constant 10))
      it "parses explicit positional argument width" do
        parseFormatArgument ":3$x" `shouldBe` Right (arg (Selector (Index 3)))
      it "parses named argument width" do
        parseFormatArgument ":width$x" `shouldBe` Right (arg (Selector (Name "width")))
    describe "argument precision" do
      let arg p = FormatArgument
            { selector = Index 0
            , style = defaultFormatStyle { precision = Just p }
            , kind = "e"
            }
      it "parses constant precision" do
        parseFormatArgument ":.10e" `shouldBe` Right (arg (Constant 10))
      it "parses explicit positional argument precision" do
        parseFormatArgument ":.3$e" `shouldBe` Right (arg (Selector (Index 3)))
      it "parses implicit positional argument precision" do
        parseFormatArgument ":.*e" `shouldBe` Right (arg (Selector (Index 1)))
      it "parses named argument precision" do
        parseFormatArgument ":.precision$e" `shouldBe` Right (arg (Selector (Name "precision")))
    it "rejects parameters in wrong order" do
      parseFormatArgument ":#+0" `shouldBe` Left
        "at offset 2: unexpected extra text in format argument: +0"
    it "allows trailing whitespace" do
      parseFormatArgument ":+#0?  " `shouldBe` Right
        FormatArgument
        { selector = Index 0
        , style = defaultFormatStyle
          { signPlus = True
          , alternate = True
          , signZero = True
          }
        , kind = "?"
        }
