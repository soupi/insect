-- | This module defines the AST for Insect.
module Insect.Language
  ( Identifier
  , Func(..)
  , BinOp(..)
  , Expression(..)
  , Value(..)
  , Rep(..)
  , Command(..)
  , Statement(..)
  ) where

import Data.Generic (class Generic, gEq, gShow)
import Prelude hiding (Unit)

-- | Type synonym for identifiers (variable names).
type Identifier = String

-- | All available mathematical functions.
data Func
  = Complement

derive instance eqFunc ∷ Eq Func
derive instance genericFunc ∷ Generic Func
instance showFunc ∷ Show Func where show = gShow

-- | Binary operators.
data BinOp
  = Add
  | Sub
  | Mul
  | And
  | Or
  | Xor
  | Shr
  | Shl
  | Sar
  | Sal
  | ConvertTo

derive instance eqBinOp ∷ Eq BinOp
derive instance genericBinOp ∷ Generic BinOp
instance showBinOp ∷ Show BinOp where
  show = gShow

-- | A mathematical expression.
data Expression
  = Scalar Value
  | Variable Identifier
  | Unit Rep
  | Apply Func Expression
  | BinOp BinOp Expression Expression

derive instance eqExpression ∷ Eq Expression
instance showExpression ∷ Show Expression where
  show (Scalar n)     = "(Scalar " <> show n <> ")"
  show (Unit rep)     = "(Unit " <> show rep <> ")"
  show (Variable n)   = "(Variable " <> show n <> ")"
  show (Apply fn x)   = "(Apply " <> show fn <> " " <> show x <> ")"
  show (BinOp op x y) = "(BinOp " <> show op <> " " <> show x <> " " <> show y <> ")"

newtype Value = Value
  { value :: Int
  , rep :: Rep
  }

data Rep
  = Binary
  | Hex
  | Decimal

derive instance genericRep ∷ Generic Rep
instance showRep ∷ Show Rep where
  show = gShow

instance eqRep ∷ Eq Rep where
  eq = gEq

derive instance genericValue ∷ Generic Value
instance showValue ∷ Show Value where
  show = gShow

instance eqValue :: Eq Value where
  eq (Value v1) (Value v2) = v1.value == v2.value

instance ordValue :: Ord Value where
  compare (Value v1) (Value v2) = compare v1.value v2.value

-- | A command in Insect.
data Command
  = Help
  | Reset
  | List
  | Clear

derive instance eqCommand ∷ Eq Command
derive instance genericCommand ∷ Generic Command
instance showCommand ∷ Show Command where show = gShow

-- | A statement in Insect.
data Statement
  = Expression Expression
  | Assignment Identifier Expression
  | Command Command

derive instance eqStatement ∷ Eq Statement
instance showStatement ∷ Show Statement where
  show (Expression e)   = "(Expression " <> show e <> ")"
  show (Assignment i e) = "(Assignment " <> show i <> " " <> show e <> ")"
  show (Command c)      = "(Command " <> show c <> ")"
