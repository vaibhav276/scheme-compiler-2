module ParserTests
  (parserUnitTests)
where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC

import           Data.Maybe
import           Parser

parserUnitTests = testGroup "Parser Unit Tests"
  [ positiveUnitTests
  , negativeUnitTests
  ]

negativeUnitTests = testGroup "Negative Unit Tests"
  [testCase "Parse: <blank>" (assertBool "" ( isJust ( readExpr "" ) ) )
  ]

positiveUnitTests = testGroup "Positive Unit Tests"
  [testCase "Parse: $" $ ( readExpr "$" ) @?= Nothing
  , testCase "Parse: <spaces>$" $ ( readExpr "  $" ) @?= Nothing
  , testCase "Parse: a" $ ( readExpr "a" ) @?= Nothing
  , testCase "Parse: \"this is a string\"" $ ( readExpr "\"this is a string\"" ) @?= Nothing
  , testCase "Parse: 25" $ ( readExpr "25" ) @?= Nothing
  , testCase "Parse: (a)" $ ( readExpr "(a)" ) @?= Nothing
  , testCase "Parse: (a test)" $ ( readExpr "(a test)" ) @?= Nothing
  , testCase "Parse: (a (nested) test)" $ ( readExpr "(a (nested) test)" ) @?= Nothing
  , testCase "Parse: (a (dotted . list) test)" $ ( readExpr "(a (dotted . list) test)" ) @?= Nothing
  , testCase "Parse: '(quoted (list))" $ ( readExpr "'(quoted (list))" ) @?= Nothing
  , testCase "Parse: '(quoted (a b c))" $ ( readExpr "'(quoted (a b c))" ) @?= Nothing
  , testCase "Parse: ('(quoted (a b c)))" $ ( readExpr "('(quoted (a b c)))" ) @?= Nothing
  , testCase "Parse: (a '(quoted (a b c)))" $ ( readExpr "(a '(quoted (a b c)))" ) @?= Nothing
  , testCase "Parse: (a '(quoted (a b c)) test)" $ ( readExpr "(a '(quoted (a b c)) test)" ) @?= Nothing
  , testCase "Parse: ( a '(quoted (a b c)) test)" $ ( readExpr "( a '(quoted (a b c)) test)" ) @?= Nothing
  ]
