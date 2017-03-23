-- | This module defines the parser for the Insect language.
module Insect.Parser
  ( parseInsect
  ) where

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (filter, length, mapWithIndex, reverse, singleton, some)
import Data.Either (Either)
import Data.Foldable (foldr, sum)
import Data.Int (fromString, fromStringAs, hexadecimal, pow)
import Data.List (List, many, init, last)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|), foldl1)
import Data.String (fromCharArray)
import Data.Traversable (traverse)
import Insect.Language (BinOp(..), Command(..), Expression(..), Func(..), Rep(..), Statement(..), Value(..))
import Text.Parsing.Parser (ParserT, Parser, ParseError, runParser, fail)
import Text.Parsing.Parser.Combinators (try, (<?>))
import Text.Parsing.Parser.String (char, eof, oneOf, string)
import Text.Parsing.Parser.Token (GenLanguageDef(..), LanguageDef, TokenParser, digit, letter, makeTokenParser)
import Prelude hiding (degree)

-- | A type synonym for the main Parser type with `String` as input.
type P a = Parser String a

-- | Possibler characters for identifiers (not for the first character).
identLetter ∷ P Char
identLetter = letter <|> digit <|> char '_' <|> char '\''

-- | The language definition.
insectLanguage ∷ LanguageDef
insectLanguage = LanguageDef
  { commentStart: ""
  , commentEnd: ""
  , commentLine: "#"
  , nestedComments: false
  , identStart: letter <|> char '_'
  , identLetter: identLetter
  , opStart: oneOf ['+', '-', '*', '·', '=', '|', '&', '#']
  , opLetter: oneOf ['>', '*']
  , reservedNames: ["help", "?", "list", "ls", "reset", "clear"]
  , reservedOpNames: ["->", "+", "-", "*", "×", "="]
  , caseSensitive: true
  }

-- | The actual token parser.
token ∷ TokenParser
token = makeTokenParser insectLanguage

-- | Parse something, inside of parens.
parens ∷ forall a. P a -> P a
parens = token.parens

-- | Parse one of the reserved operators.
reservedOp ∷ String -> P Unit
reservedOp = token.reservedOp

-- | Parse a reserverd keyword.
reserved ∷ String -> P Unit
reserved = token.reserved

-- | Parse zero or more whitespace characters.
whiteSpace ∷ P Unit
whiteSpace = token.whiteSpace

-- | Parse a value.
number ∷ P Value
number = do
  int <- digits

  whiteSpace
  let num = fromString int

  case num of
    Just i  -> pure $ Value { value: i, rep: Decimal }
    Nothing -> fail $ "readInt failed for input '" <> int <> "'"

  where
    digits ∷ P String
    digits = do
      ds <- filter (_ /= '\'') <$> some (oneOf ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9','\''] <?> "a digit")
      pure $ fromCharArray $ filter (_ /= '\'') ds

-- | Parse a binary number.
binNumber ∷ P Value
binNumber = do
  void $ char 'b'
  ds <-  filter (_ /= '\'') <$> some (oneOf binDigit <?> "a binary digit")
  when (length ds > 32) $
    fail $ "Binary numbers are limited to 32 bits which means 32 binary digits long. '\\b" <> fromCharArray ds <> "' is "
      <> show (length ds) <> " digits long."

  case traverse (fromStringAs hexadecimal <<< fromCharArray <<< singleton) ds of
    Nothing ->
      fail $ "Failed to read binary number: \\b" <> fromCharArray ds
    Just nm ->
      pure
        <<< Value
        <<< { value: _, rep: Binary }
        <<< sum
        <<< mapWithIndex (\i d -> d * (2 `pow` i))
        $ reverse nm

-- | Parse a hexadecimal digit.
binDigit :: Array Char
binDigit =
  [ '0'
  , '1'
  , '\''
  ]


-- | Parse a hexadecimal number.
hexNumber ∷ P Value
hexNumber = do
  void $ char 'x'
  ds <- filter (_ /= '\'') <$> some (oneOf hexDigit <?> "a hexadecimal digit")
  when (length ds > 8) $
    fail $ "Hexadecimal numbers are limited to 32 bits which means 8 hexadecimal digits long. '\\x" <> fromCharArray ds <> "' is "
      <> show (length ds) <> " digits long."

  case traverse (fromStringAs hexadecimal <<< fromCharArray <<< singleton) ds of
    Nothing ->
      fail $ "Failed to read hexadecimal number: \\x" <> fromCharArray ds
    Just nm ->
      pure
        <<< Value
        <<< { value: _, rep: Hex }
        <<< sum
        <<< mapWithIndex (\i d -> d * (16 `pow` i))
        $ reverse nm

-- | Parse a hexadecimal digit.
hexDigit :: Array Char
hexDigit =
  [ '0'
  , '1'
  , '2'
  , '3'
  , '4'
  , '5'
  , '6'
  , '7'
  , '8'
  , '9'
  , 'a'
  , 'b'
  , 'c'
  , 'd'
  , 'e'
  , 'f'
  , 'A'
  , 'B'
  , 'C'
  , 'D'
  , 'E'
  , 'F'
  , '\''
  ]




-- | Parse the name of a variable, like `my_variable'`.
variable ∷ P Expression
variable = Variable <$> token.identifier

-- | Parse the name of a mathematical function.
binOpName ∷ P BinOp
binOpName =
      (string "and" *> pure And)
  <|> (string "or"  *> pure Or)
  <|> (string "xor" *> pure Xor)
  <|> (string "add" *> pure Add)
  <|> (string "sub" *> pure Sub)
  <|> (string "mul" *> pure Mul)
  <|> (string "shr" *> pure Shr)
  <|> (string "shl" *> pure Shl)
  <|> (string "sar" *> pure Sar)
  <|> (string "sal" *> pure Sal)

-- | Parse the name of a mathematical function.
funcName ∷ P Func
funcName =
      (string "complement" *> pure Complement)
  <|> (string "negate" *> pure Negate)

-- | Parse a representation unit.
rep :: P Rep
rep =
  (string "binary" $> Binary)
  <|> (string "hex" $> Hex)
  <|> (string "decimal" $> Decimal)

-- | A version of `sepBy1` that returns a `NonEmpty List`.
sepBy1 ∷ forall m s a sep. Monad m ⇒ ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmpty List a)
sepBy1 p sep = do
  a <- p
  as <- many $ do
    sep
    p
  pure (a :| as)

-- | Fold a non-empty structure, collecting results using a binary operation.
foldr1 ∷ forall a. (a -> a -> a) -> NonEmpty List a -> a
foldr1 f (a :| xs) =
  case init xs, last xs of
    Just bs, Just b -> f a (foldr f b bs)
    _, _ -> a

-- | Parse a full mathematical expression.
expression ∷ P Expression
expression =
  fix \p ->
    let
      atomic ∷ P Expression
      atomic = fix \a ->
        whiteSpace *> (
              parens p
          <|> (Scalar <$> (char '\\' *> (hexNumber <|> binNumber)))
          <|> (Scalar <$> number)
          <|> try (Unit <$> rep)
          <|> try (BinOp <$> binOpName <*> a <*> a)
          <|> try (Apply <$> funcName <*> (whiteSpace *> a))
          <|> variable
          ) <* whiteSpace

      sepByMul ∷ P Expression
      sepByMul = foldl1 (BinOp Mul) <$> atomic `sepBy1` mulOp

      sepBySub ∷ P Expression
      sepBySub = foldl1 (BinOp Sub) <$> sepByMul `sepBy1` subOp

      sepByAdd ∷ P Expression
      sepByAdd = foldl1 (BinOp Add) <$> sepBySub `sepBy1` addOp

      sepByConv ∷ P Expression
      sepByConv = foldl1 (BinOp ConvertTo) <$> sepByAdd `sepBy1` arrOp

    in sepByConv

  where

    mulOp = reservedOp "*"
    subOp = reservedOp "-"
    addOp = reservedOp "+"
    arrOp = reservedOp "->"

-- | Parse a mathematical expression like `\b11+1 -> hex`.
fullExpression ∷ P Expression
fullExpression = do
  whiteSpace
  expr <- expression
  eof <?> "end of input"

  pure $ expr

-- | Parse an Insect command.
command ∷ P Command
command =
  (
        (reserved "help" <|> reserved "?") *> pure Help
    <|> (reserved "list" <|> reserved "ls") *> pure List
    <|> (reserved "reset") *> pure Reset
    <|> (reserved "clear") *> pure Clear
  ) <* eof

-- | Parse a variable assignment like `x = 3m*pi`
assignment ∷ P Statement
assignment = do
  whiteSpace
  var <- token.identifier
  reservedOp "="
  value <- expression
  pure $ Assignment var value

-- | Parse a statement in the Insect language.
statement ∷ P Statement
statement =
      (Command <$> command)
  <|> try assignment
  <|> (Expression <$> fullExpression)

-- | Run the Insect-parser on a `String` input.
parseInsect ∷ String -> Either ParseError Statement
parseInsect inp = runParser inp statement
