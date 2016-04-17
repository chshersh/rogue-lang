import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = defaultMainWithOpts
       [ testCase "foo" (assertEqual "foo test" 1 1)
       , testCase "bar" (assertEqual "bar test" 1 2)
       ] mempty