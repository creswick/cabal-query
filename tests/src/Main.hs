module Main where

import Test.Tasty ( TestTree, defaultIngredients
                  , defaultMainWithIngredients, testGroup)
import Test.Tasty.Ingredients ( Ingredient)
import Test.Tasty.Runners.AntXML ( antXMLRunner)

import Tests.Integration

main :: IO ()
main = defaultMainWithIngredients ingredients tests

ingredients :: [Ingredient]
ingredients = antXMLRunner : defaultIngredients

tests :: TestTree
tests = testGroup "Tests"
  [ integrationTests
  ]

