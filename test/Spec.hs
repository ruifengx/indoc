{-# LANGUAGE BlockArguments #-}

import Test.Hspec

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
    it "drops a leading empty line" do
      unindent "\nit gets dropped" `shouldBe` "it gets dropped"
    it "drops only one leading empty line" do
      unindent "\n\nthe second is kept" `shouldBe` "\nthe second is kept"
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
