module Insect.Environment
  ( Environment
  , initialEnvironment
  , minInt
  , maxInt
  ) where

import Prelude
import Data.StrMap (StrMap, fromFoldable)
import Data.Tuple (Tuple(Tuple))
import Insect.Language (Rep(..), Value(..))

-- | The environment consists of identifiers that are mapped to specific
-- | quantities.
type Environment = StrMap Value

initialEnvironment ∷ Environment
initialEnvironment = fromFoldable
  [ Tuple "maxInt" $ Value { value: maxInt, rep: Decimal }
  , Tuple "minInt" $ Value { value: minInt, rep: Decimal }
  , Tuple "minus1" $ Value { value: -1, rep: Decimal }
  ]

maxInt :: Int
maxInt = 2147483647

minInt :: Int
minInt = -2147483648
