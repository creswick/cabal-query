module Tests.Integration where

import Test.Tasty ( TestTree, testGroup)
import Test.Tasty.HUnit

integrationTests :: TestTree
integrationTests = testGroup "Integration Tests"
                   [ testCase "Exit failure with no cabal files."
                      testNoCabalFiles
                   ]


testNoCabalFiles :: Assertion
testNoCabalFiles = assertFailure "fail on purpose."
