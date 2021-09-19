module Language.Mimsa.Typechecker.OutputTypes (getExpressionSourceItems) where

import Data.Text (Text)
import Language.Mimsa.ExprUtils
import Language.Mimsa.Printer
import Language.Mimsa.Project.SourceSpan
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project.SourceItem
import Language.Mimsa.Types.Typechecker

-- return types inside spans for server

getExpressionSourceItems :: Text -> Expr Variable TypedAnnotation -> [SourceItem]
getExpressionSourceItems input = withMonoid fn
  where
    fn expr =
      let monoType = getTypeFromAnn expr
          sSpan =
            sourceSpan
              input
              (getAnnotationForType monoType)
       in case sSpan of
            Just sSpan' ->
              (True, [SourceItem (prettyPrint monoType) sSpan'])
            Nothing -> (True, mempty)
