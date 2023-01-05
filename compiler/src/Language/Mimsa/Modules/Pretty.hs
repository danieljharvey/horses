{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Modules.Pretty (modulePretty) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Language.Mimsa.Core
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Types.Typechecker
import Prettyprinter

-- | display types for module in a nice way
modulePretty :: Module (Type Annotation) -> Doc a
modulePretty mod' =
  let useful = filterExported mod'
      prettyExp (k, a) = indentMulti 2 (prettyDoc k <> ":" <+> prettyDoc (getTypeFromAnn a))
      prettyType (k, a) = indentMulti 2 (prettyDoc k <> ":" <+> prettyDoc a)
   in align
        ( encloseSep
            lbrace
            rbrace
            comma
            ( ( prettyExp
                  <$> M.toList (moExpressions useful)
              )
                <> ( prettyType
                       <$> M.toList (moDataTypes useful)
                   )
            )
        )

filterByKey :: (k -> Bool) -> Map k a -> Map k a
filterByKey f = M.filterWithKey (\k _ -> f k)

filterExported :: Module ann -> Module ann
filterExported mod' =
  mod'
    { moDataTypes =
        filterByKey
          (\k -> S.member k (moDataTypeExports mod'))
          (moDataTypes mod'),
      moExpressions =
        filterByKey
          (\k -> S.member k (moExpressionExports mod'))
          (moExpressions mod')
    }
