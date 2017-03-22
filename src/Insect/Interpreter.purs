-- | This module defines the interpreter for Insect.
module Insect.Interpreter
  ( MessageType(..)
  , Message(..)
  , runInsect
  ) where

import Data.Bifunctor (lmap)
import Data.Either (Either(Right, Left))
import Data.Foldable (intercalate)
import Data.Int (binary, decimal, hexadecimal, toStringAs)
import Data.Int.Bits (and, complement, or, shl, shr, xor)
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup, insert, foldMap)
import Debug.Trace (traceShow)
import Insect.Environment (Environment, initialEnvironment)
import Insect.Language (BinOp(..), Command(..), Expression(..), Func(..), Rep(..), Statement(..), Value(..))
import Prelude hiding (degree)

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
      Add       -> overValue (+)
      Sub       -> overValue (-)
      Mul       -> overValue (*)
      And       -> overValue and
      Or        -> overValue or
      Xor       -> overValue xor
      Shr       -> overValue shr
      Shl       -> overValue shl
      Sar       -> overValue \x y ->
        let signBit = shr 31 x == 1
            temp = shr in x + y
      Sal       -> overValue (+)
      ConvertTo -> overValue (const id)

overValue :: (Int -> Int -> Int) -> Value -> Value -> Value
overValue fn (Value v1) (Value v2) =
  Value $ v1 { value = fn v1.value v2.value }

-- | Apply a function to a value.
applyFunction ∷ Func -> Value -> Value
applyFunction fn (Value v) =
    case fn of
      Complement -> Value $ v { value = complement v.value }
 
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
    Binary -> "\\b" <> toStringAs binary value
    Hex -> "\\x" <> toStringAs hexadecimal value
    Decimal -> toStringAs decimal value
        

-- | Run a single statement of an Insect program.
runInsect ∷ Environment -> Statement -> Response
runInsect env (Expression e) = message Val env (eval env (traceShow e (const e)))
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
