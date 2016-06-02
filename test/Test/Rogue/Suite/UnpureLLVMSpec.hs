module Test.Rogue.Suite.UnpureLLVMSpec
    ( spec
    ) where

import           Test.Hspec                    (Spec, describe, it, shouldReturn)

import           Test.Rogue.Runner.ProcessLLVM (createProcessLLVM)

spec :: Spec
spec =
    describe "Rogue" $ do
        it "returns sum of 5 and 9 (should be 14)" $ do
            createProcessLLVM `shouldReturn` 14