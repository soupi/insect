module Insect.Environment
  ( Environment
  , initialEnvironment
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
  [ Tuple "maxInt" $ Value { value: 217483647, rep: Decimal }
  , Tuple "minInt" $ Value { value: -217483648, rep: Decimal }
  , Tuple "minus1" $ Value { value: -1, rep: Decimal }
  ]
