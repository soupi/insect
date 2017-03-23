-- | This module defines the interpreter for Insect.
module Insect.Interpreter
  ( MessageType(..)
  , Message(..)
  , runInsect
  ) where

import Prelude
import Data.Array (mapWithIndex, replicate, reverse)
import Data.Bifunctor (lmap)
import Data.Either (Either(Right, Left))
import Data.Foldable (intercalate, sum)
import Data.Int (binary, decimal, fromStringAs, hexadecimal, pow, toStringAs)
import Data.Int.Bits (and, complement, or, xor)
import Data.Maybe (Maybe(..), fromJust)
import Data.StrMap (lookup, insert, foldMap)
import Data.String (drop, fromCharArray, length, take, toCharArray)
import Insect.Environment (Environment, initialEnvironment, maxInt, minInt)
import Insect.Language (BinOp(..), Command(..), Expression(..), Func(..), Rep(..), Statement(..), Value(..))
import Partial.Unsafe (unsafePartial)

-- | Types of errors that may appear during evaluation.
data EvalError
  = EvaluationError String
  | LookupError String

-- | A type synonym for error handling. A value of type `Expect Number` is
-- | expected to be a number but might also result in an evaluation error.
type Expect = Either EvalError

-- | Output types for highlighting.
data MessageType = Val | ValSet | Info | Error | Other

-- | The output type of the interpreter.
data Message = Message MessageType String

-- | Apply a binary operation to a value.
applyBinOp ∷ BinOp -> Value -> Value -> Value
applyBinOp fn v1 v2 = run fn v1 v2
  where
    run = case _ of
      Add -> overValue (+)
      Sub -> overValue (-)
      Mul -> overValue (*)
      And -> overValue and
      Or  -> overValue or
      Xor -> overValue xor
      Shr -> overValue myShr
      Shl -> overValue myShl
      Sar -> overValue \x y ->
        if x < 0
          then myShr x y `or` genBinaryOnesL y
          else myShr x y `and` maxInt

      Sal -> overValue \x y ->
        if x < 0
          then myShl x y `or` myShl 1 31
          else myShl x y `and` maxInt

      ConvertTo -> overValue (const id)

myShl :: Int -> Int -> Int
myShl x y
  | y == 0 = x
  | y > 0 =
    let x' = (_ <> (fromCharArray $ replicate y '0')) $ drop (2 + y) $ prettyPrint $ Value { value: x, rep: Binary }
    in readBinary x'
  | otherwise = myShr x (-y)

myShr :: Int -> Int -> Int
myShr x y
  | y == 0 = x
  | y > 0 =
    let x' = ((fromCharArray $ replicate y '0') <> _) $ take (32 - y) $ drop 2 $ prettyPrint $ Value { value: x, rep: Binary }
    in readBinary x'
  | otherwise = myShl x (-y)

genBinaryOnesR :: Int -> Int
genBinaryOnesR n =
  sum
  <<< mapWithIndex (\i d -> d * (2 `pow` i))
  $ replicate n 1

genBinaryOnesL :: Int -> Int
genBinaryOnesL n =
  sum
  <<< mapWithIndex (\i d -> d * (2 `pow` i))
  $ replicate (32 - n) 0 <> replicate n 1

readBinary :: String -> Int
readBinary s =
  sum
  <<< mapWithIndex (\i d -> d * (2 `pow` i))
  <<< reverse
  <<< map (\c -> unsafePartial $ fromBinaryChar c)
  $ toCharArray s

fromBinaryChar :: Partial => Char -> Int
fromBinaryChar = case _ of
  '0' -> 0
  '1' -> 1

overValue :: (Int -> Int -> Int) -> Value -> Value -> Value
overValue fn (Value v1) (Value v2) =
  Value $ v1 { value = fn v1.value v2.value }

-- | Apply a function to a value.
applyFunction ∷ Func -> Value -> Value
applyFunction fn (Value v) =
    case fn of
      Complement -> Value $ v { value = complement v.value }
      Negate -> Value $ v { value = negate v.value }
 
-- | Evaluate an expression
eval ∷ Environment -> Expression -> Expect Expression
eval env (Scalar v)      = pure (Scalar v)
eval env (Unit u)        = pure (Unit u)
eval env (Variable name) =
  case lookup name env of
    Just v -> pure (Scalar v)
    Nothing -> Left (LookupError name)
eval env (Apply fn x)    = eval env x >>= case _ of
       Scalar v -> pure $ Scalar $ applyFunction fn v
       other -> Left $ EvaluationError $ "Could not apply a function to an expression which is not reduced to a scalar.\nThe evaluation of the expression is: " <> show other
eval env (BinOp op x y)  = do
  x' <- eval env x
  y' <- eval env y
  case { op: op, arg1: x', arg2: y' } of
    { op: ConvertTo, arg1: Scalar v1, arg2: Unit r } ->
      pure $ Scalar $ applyBinOp Add (Value { value: 0, rep: r }) v1

    { arg1: Scalar v1, arg2: Scalar v2 } ->
      pure $ Scalar $ applyBinOp op v1 v2

    other ->
      Left $ EvaluationError $ "Could not apply a function to expressions which are not reduced to scalars.\nThe evaluation of the expressions is: "
        <> show other.arg1
        <> " and "
        <> show other.arg2
  where
    wrap ∷ forall a. Either String a -> Either EvalError a
    wrap = lmap EvaluationError

-- | Get the error message for an evaluation error.
evalErrorMessage ∷ EvalError -> String
evalErrorMessage (EvaluationError e) = e
evalErrorMessage (LookupError name) = "Unknown variable '" <> name <> "'"

-- | Interpreter return type
type Response = { msg ∷ Message, newEnv ∷ Environment }

-- | Helper to construct an interpreter response
message ∷ MessageType -> Environment -> Expect Expression -> Response
message _ env (Left e) =
  { msg: Message Error (evalErrorMessage e)
  , newEnv: env
  }
message mt env (Right (Scalar v)) =
  { msg: Message mt (prettyPrint v)
  , newEnv: insert "ans" v env
  }
message _ env (Right other) =
  { msg: Message Error $ "Result of the expression is not a scalar: " <> show other
  , newEnv: env
  }

prettyPrint :: Value -> String
prettyPrint (Value { value, rep }) =
  case rep of
    Decimal -> toStringAs decimal value

    Binary -> "\\b" <>
      if value >= 0
        then
          let s = toStringAs binary value
          in fromCharArray (replicate (32 - length s) '0') <> s
        else
          if value == minInt
            then fromCharArray $ ['1'] <> replicate 31 '0'
            else
              let s = toStringAs binary (maxInt + value + 1)
              in "1" <> fromCharArray (replicate (31 - length s) '0') <> s 

    Hex -> "\\x" <>
      if value >= 0
        then
          let s = toStringAs hexadecimal value
          in fromCharArray (replicate (8 - length s) '0') <> s
        else
          let str = toStringAs hexadecimal (maxInt + value + 1)
          in
            if length str < 8
              then "8" <> fromCharArray (replicate (8 - length str - 1) '0') <> str
              else toStringAs hexadecimal ((unsafePartial $ fromJust $ fromStringAs hexadecimal $ take 1 str) `or` 8) <> drop 1 str


-- | Run a single statement of an Insect program.
runInsect ∷ Environment -> Statement -> Response
runInsect env (Expression e) = message Val env (eval env e)
runInsect env (Assignment n v) =
  case eval env v of
    Left evalErr -> message Error env (Left evalErr)
    Right (Scalar value) -> message ValSet (insert n value env) (Right (Scalar value))
    Right value -> message Other env (Right value)
runInsect env (Command Help) = { msg: Message Other (intercalate "\n"
  [ ""
  , "*binsect* evaluates binary expressions and calculations"
  , ""
  , "You can start by trying one of these examples:"
  , ""
  , "  > `\\b101 + \\xa`             > `shl \\b101 1`"
  , ""
  , "  > `complent 0 -> hex`       > `\\xff - \\b1111'1111`"
  , ""
  , "  > `minInt`                  > `maxInt`"
  , ""
  , "More information: https://github.com/soupi/insect"
  ]), newEnv : env }
runInsect env (Command List) =
  { msg: Message Other list
  , newEnv: env }
  where
    list = "List of variables:\n" <> foldMap toLine env
    toLine k v = "\n  * " <> k <> " = `" <> prettyPrint v <> "`"
runInsect env (Command Reset) =
  { msg: Message Info "Environment has been reset"
  , newEnv: initialEnvironment }
runInsect env (Command _) = { msg: Message Error "???", newEnv: env }
