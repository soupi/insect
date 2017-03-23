module Test.Main where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (Either(..))
import Data.Foldable (traverse_)

import Insect (repl)
import Insect.Environment (Environment)
import Insect.Language (BinOp(ConvertTo, Sub, Add, Mul), Expression(Scalar, Variable, Unit, BinOp), Rep(Binary, Hex, Decimal), Statement(Assignment, Expression), Value(Value))
import Insect.Parser (parseInsect)
import Test.Unit (suite, test, failure)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Text.Parsing.Parser (parseErrorMessage, parseErrorPosition)
import Text.Parsing.Parser.Pos (Position(..))
import Prelude hiding (degree)

shouldParseAs ∷ forall eff. Statement -> String -> Aff eff Unit
shouldParseAs expected input =
  case parseInsect input of
    Left err ->
      case parseErrorPosition err of
        (Position pos) ->
          failure $ "Parse error for input '" <> input <> "': "
                                <> parseErrorMessage err
                                <> " at position "
                                <> show pos.column
    Right output ->
      unless (output == expected) $ do
        failure $ "Unexpected result:\n" <>
                  "Input:    '" <> input <> "'\n" <>
                  "Output:   " <> show output <> "\n" <>
                  "Expected: " <> show expected <> "\n"

allParseAs ∷ forall eff. Statement -> Array String -> Aff eff Unit
allParseAs expected = traverse_ (shouldParseAs expected)

shouldFail ∷ forall eff. String -> Aff eff Unit
shouldFail input = do
  case parseInsect input of
   Left _ -> pure unit
   Right output -> failure $ "input is expected to throw a parse error: '" <> input <> "'"

expectOutput ∷ forall eff. Environment -> String -> String -> Aff eff Unit
expectOutput env expected inp =
  let res = repl env inp
      out = res.msg
  in
    unless (out == expected) do
      failure $ "Unexpected result:\n" <>
                "Input:    '" <> inp <> "'\n" <>
                "Output:   '" <> out <> "'\n" <>
                "Expected: '" <> expected <> "'\n"


main ∷ Eff (console ∷ CONSOLE, testOutput ∷ TESTOUTPUT, avar ∷ AVAR) Unit
main = runTest do
  -- Helper to construct quantities
  let q s u = BinOp Mul (Scalar s) (Unit u)

  suite "Parser - Numbers" do
    test "Simple numbers" do
      allParseAs (Expression (Scalar $ decV 1))
        [ "1"
        , "  1  "
        ]

      shouldFail "123.."
      shouldFail "0.."
      shouldFail ".0."
      shouldFail "."
      shouldFail ".2"

    test "Large numbers" do
      allParseAs (Expression (Scalar $ decV 2147483647))
        [ "2147483647"
        , "\\b0111'1111'1111'1111'1111'1111'1111'1111"
        , "\\x7fff'ffff"
        ]

      allParseAs (Expression (BinOp Add (Scalar $ decV 4) (Scalar $ decV 3))) $
        [ "4+3"
        , "4 + 3"
        ]

    test "Multiplication" do
      allParseAs (Expression (BinOp Mul (Scalar $ decV 5) (Scalar $ decV 3))) $
        [ "5*3"
        , " 5 * 3 "
        , " ( 5 ) * ( 3 ) "
        , " ( ( 5 ) * ( 3 ) ) "
        , " ( 5 * 3 ) "
        ]

      shouldFail "5*"

    test "Addition" do
      allParseAs (Expression (BinOp Add (Scalar $ decV 5) (Scalar $ decV 3))) $
        [ "5+3"
        , " 5 + 3 "
        , " ( 5 ) + ( 3 ) "
        , " ( ( 5 ) + ( 3 ) ) "
        , " ( 5 + 3 ) "
        ]

      shouldFail "3 + "
      shouldFail "3 + @"

    test "Subtraction" do
      allParseAs (Expression (BinOp Sub (Scalar $ decV 5) (Scalar $ decV 3))) $
        [ "5-3"
        , " 5 - 3 "
        , " ( 5 ) - ( 3 ) "
        , " ( ( 5 ) - ( 3 ) ) "
        , " ( 5 - 3 ) "
        ]

      shouldFail "3 - "


  suite "Parser - Conversions" do
    test "Simple" do
      allParseAs (Expression (BinOp ConvertTo (Scalar $ decV 2) (Unit Binary)))
        [ "2 -> binary"
        , "  2->binary "
        , "  2  ->  binary "
        ]

    test "Functions + Conversions" do
      allParseAs (Expression (BinOp ConvertTo (BinOp Add (Scalar $ decV 1) (Scalar $ decV 2)) (Unit Binary)))
        [ "add 1 2 -> binary"
        , "(add 1 2) -> binary"
        , "(add ( 1) 2) -> binary"
        ]

      shouldFail "2->"

  suite "Parser - Identifiers" do
    test "Valid and invalid names" do
      shouldParseAs (Expression (Variable "x")) "x"
      shouldParseAs (Expression (Variable "µ")) "µ"
      shouldParseAs (Expression (Variable "pi")) "pi"
      shouldParseAs (Expression (Variable "x_2")) "x_2"
      shouldParseAs (Expression (Variable "länge")) "länge"
      shouldParseAs (Expression (Variable "_prefixed")) "_prefixed"
      shouldParseAs (Expression (Variable "x'")) "x'"
      shouldParseAs (Expression (Variable "t''")) "t''"

      shouldFail "xs,as"
      shouldFail "hello$"

    test "Variables which begin like units" do
      shouldParseAs (Expression (Variable "myVariable")) "myVariable"
      shouldParseAs (Expression (Variable "density")) "density"

  suite "Parser - Assignments" do
    test "Simple" do
      allParseAs (Assignment "xyz_123" (Scalar $ decV 1)) $
        [ "xyz_123 = 1"
        , "xyz_123=1"
        , "  xyz_123  =  1  "
        ]

      shouldFail "x+y=3"
      shouldFail "x+2=3"
      shouldFail "3=5"
      shouldFail "x="


decV :: Int -> Value
decV v = Value { value: v, rep: Decimal }

hexV :: Int -> Value
hexV v = Value { value: v, rep: Hex }

binV :: Int -> Value
binV v = Value { value: v, rep: Binary }
