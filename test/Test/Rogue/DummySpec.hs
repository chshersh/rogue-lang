module Test.Rogue.DummySpec
    ( spec
    ) where

import           Control.Exception (evaluate)

import           Test.Hspec        (Spec, anyException, describe, it,
                                    shouldBe, shouldThrow)
import           Test.QuickCheck   (property)

spec :: Spec
spec =
    describe "Prelude.head" $ do
        it "returns the first element of a list" $ do
            head [23 ..] `shouldBe` (23 :: Int)

        it "returns the first element of an *arbitrary* list" $
            property $ \x xs -> head (x:xs) == (x :: Int)

        it "throws an exception if used with an empty list" $ do
            evaluate (head []) `shouldThrow` anyException