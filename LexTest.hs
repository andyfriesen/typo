{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module LexTest where

import Prelude hiding (lex)

import Lex

import Test.Tasty
import Test.Tasty.HUnit (testCase)
-- import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH
import Test.HUnit (Assertion, assertEqual)
-- import Test.QuickCheck
-- import Test.QuickCheck.Instances ()

case_empty :: Assertion
case_empty =
    assertEqual "No tokens" (Right []) (lex "")

case_1_keyword :: Assertion
case_1_keyword =
    assertEqual "One keyword" (Right [TKeyword KModule]) (lex "module")

case_2_keywords :: Assertion
case_2_keywords =
    assertEqual "Two keywords" (Right [TKeyword KModule, TKeyword KExport]) (lex "module export")

case_operators :: Assertion
case_operators =
    assertEqual
        "Operators"
        (Right
            [ TOperator OShiftRightEquals
            , TOperator ODoublePipe
            , TOperator OBang
            ])
        (lex ">>=||!")

case_identifiers :: Assertion
case_identifiers =
    assertEqual
        "Identifiers"
        (Right
            [ TKeyword KModule
            , TIdentifier "foo"
            ])
        (lex "module foo")

case_string_literal :: Assertion
case_string_literal =
    assertEqual
        "String Literal"
        (Right
            [ TStringLiteral "abc"
            ])
        (lex "\"abc\"")

case_two_string_literals :: Assertion
case_two_string_literals =
    assertEqual
        "Two strings"
        (Right
            [ TStringLiteral "abc"
            , TStringLiteral "def"
            ])
        (lex "\"abc\"\"def\"")

case_number :: Assertion
case_number =
    assertEqual
        "Number"
        (Right
            [ TNumericLiteral "12345" ])
        (lex "12345")

case_decimal :: Assertion
case_decimal =
    assertEqual
        "Decimal"
        (Right
            [ TNumericLiteral "3.14159" ])
        (lex "3.14159")

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
