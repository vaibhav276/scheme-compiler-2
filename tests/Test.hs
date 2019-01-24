import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Compiler

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [
  ]

unitTests = testGroup "Unit tests"
  [ testCase "Compile a string expression" $
      ( compile "Test expression" ) @?= Nothing
  ]
