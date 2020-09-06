module Language.Mimsa.Types.Environment where

import Data.Map (Map)
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.DataType
import Language.Mimsa.Types.Scheme
import Language.Mimsa.Types.Variable

-- everything we need in typechecking environment
data Environment
  = Environment
      { getSchemes :: Map Variable Scheme,
        getDataTypes :: Map Construct DataType
      }
  deriving (Eq, Ord, Show)

instance Semigroup Environment where
  (Environment a b) <> (Environment a' b') = Environment (a <> a') (b <> b')

instance Monoid Environment where
  mempty = Environment mempty mempty
