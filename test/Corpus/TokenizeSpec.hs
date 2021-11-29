module Corpus.TokenizeSpec where

import Data.Text
import Test.Hspec

import Repl

spec :: Spec
spec = do
  describe "tokenizing a word" $ do
    it "builds a token with no indent stack when no symmetric punctuation is included" $ do
      mkToken "foo" `shouldBe` Token "foo" 0 0 0 0

    it "builds a token with an indent stack when parens are included" $ do
      mkToken "(foo" `shouldBe` Token "(foo" 1 0 0 0

    it "builds a token with no indent stack when parens are balanced" $ do
      mkToken "(foo)" `shouldBe` Token "(foo)" 0 0 0 0
    
    it "respects asymmetric punctuation even when some is balanced" $ do
      mkToken "(foo(bar<baz)quux>" `shouldBe` Token "(foo(bar<baz)quux>" 1 0 0 0

    it "doesn't count operators" $ do
      mkToken ">>=" `shouldBe` Token ">>=" 0 0 0 0

  describe "concatenating two tokens" $ do
    it "intercalates the two Texts with a space and re-evaluates their stacks" $ do
      -- this one's a bit tricky, I probably don't _always_ want to fuse two tokens
      -- with zero stacks, but I don't know enough about the problem just yet to
      -- spec that out. let's roll with this for now
      mkToken "(foo" `catTokens` mkToken "<bar" `shouldBe` Token "(foo <bar" 1 0 0 1

  describe "fusing a prefix of tokens" $ do
    it "does nothing when all the tokens are closed" $ do
      fusePrefix (mkToken "foo") [mkToken "(bar)", mkToken "::", mkToken "baz"]
      `shouldBe`
      (mkToken "foo", [mkToken "(bar)", mkToken "::", mkToken "baz"])

    it "fuses a pair of leading tokens that complete each other" $ do
      fusePrefix (mkToken "foo(") [mkToken "bar)", mkToken "::", mkToken "baz"]
      `shouldBe`
      (mkToken "foo( bar)", [mkToken "::", mkToken "baz"])

    it "fuses past an intervening closed token or two" $ do
      fusePrefix (mkToken "foo(") [mkToken "bar", mkToken "::", mkToken "baz)"]
      `shouldBe`
      (mkToken "foo( bar :: baz)", [])

    it "fuses through mismatched punct pairs, probably suboptimal" $ do
      -- this is a typing tutor, not a language parser. if you want to train on a
      -- corpus like this be my guest I suppose. leaning on "tests as doc" here
      fusePrefix (mkToken "foo(") [mkToken "bar<", mkToken "::)", mkToken "baz>"]
      `shouldBe`
      (mkToken "foo( bar< ::) baz>", [])

    it "does nothing when it can't find a closed prefix" $ do
      fusePrefix (mkToken "foo(") [mkToken "bar", mkToken "::", mkToken "baz"]
      `shouldBe`
      (mkToken "foo(", [mkToken "bar", mkToken "::", mkToken "baz"])
      
    it "doesn't get confused by operators" $ do
      fusePrefix (mkToken "foo(") [mkToken "<<bar", mkToken ">>=", mkToken "baz"]
      `shouldBe`
      (mkToken "foo(", [mkToken "<<bar", mkToken ">>=", mkToken "baz"])

    it "doesn't create tokens longer than 20ch" $ do
      -- this is more or less a test of @isClosed@
      fusePrefix (mkToken "foo(") [mkToken "bar", mkToken "::", mkToken "baz-is-a-very-long-token)"]
      `shouldBe`
      (mkToken "foo(", [mkToken "bar", mkToken "::", mkToken "baz-is-a-very-long-token)"])

  describe "tokenizing a line" $ do
    it "returns an empty list on an empty line" $ do
      tokenize "" `shouldBe` []

    it "returns a single token if only one is present" $ do
      tokenize "foo" `shouldBe` [mkToken "foo"]

    it "returns many tokens, grouping punctuation, on a full line" $ do
      tokenize "spec :: Spec" `shouldBe` [mkToken "spec", mkToken "::", mkToken "Spec"]