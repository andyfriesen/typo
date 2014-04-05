{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module LexTest where

import Prelude hiding (lex)

import Lex

import Data.Text.Lazy (Text)

import Test.Tasty
import Test.Tasty.HUnit (testCase)
-- import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH
import Test.HUnit (Assertion, assertEqual)
-- import Test.QuickCheck
-- import Test.QuickCheck.Instances ()

assumeRight :: Show a => Either a b -> b
assumeRight (Left l) = error $ "assumeRight got a left: " ++ (show l)
assumeRight (Right r) = r

assertParse :: String -> [Token] -> Text -> Assertion
assertParse name expected source =
    assertEqual
        name
        expected
        (map lToken $ assumeRight $ lex "foo.ts" source)

case_empty :: Assertion
case_empty =
    assertParse "No tokens" [] ""

case_1_keyword :: Assertion
case_1_keyword =
    assertParse "One keyword"  [TKeyword KModule] "module"

case_2_keywords :: Assertion
case_2_keywords =
    assertParse
        "Two keywords"
        [TKeyword KModule, TKeyword KExport]
        "module export"

case_operators :: Assertion
case_operators =
    assertParse
        "Operators"
        [ TOperator OShiftRightEquals
        , TOperator ODoublePipe
        , TOperator OBang
        ]
        ">>=||!"

case_identifiers :: Assertion
case_identifiers =
    assertParse
        "Identifiers"
        [ TKeyword KModule
        , TIdentifier "foo"
        ]
        "module foo"

case_string_literal :: Assertion
case_string_literal =
    assertParse
        "String Literal"
        [ TStringLiteral "abc" ]
        "\"abc\""

case_two_string_literals :: Assertion
case_two_string_literals =
    assertParse
        "Two strings"
        [ TStringLiteral "abc"
        , TStringLiteral "def"
        ]
        "\"abc\"\"def\""

case_number :: Assertion
case_number =
    assertParse
        "Number"
        [ TNumericLiteral "12345" ]
        "12345"

case_decimal :: Assertion
case_decimal =
    assertParse
        "Decimal"
        [ TNumericLiteral "3.14159" ]
        "3.14159"

case_whitespace =
    assertParse
        "Whitespace"
        [ TKeyword KModule ]
        "\n\tmodule\t  \n"

case_block_comments :: Assertion
case_block_comments =
    assertParse
        "Block comments"
        [ TKeyword KModule
        , TIdentifier "Foo"
        ]
        "module /*la la\n la */ \n\tFoo"

case_line_comments =
    assertParse
        "Line comments"
        [ TKeyword KModule
        , TIdentifier "Bar"
        ]
        "module // this is a keyword\nBar"

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
