{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes    #-}

import Test.Hspec

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
