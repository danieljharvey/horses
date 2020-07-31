module Language.Mimsa.Project.Usages where

import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.StoreExpression
import Data.Set (Set)
import Data.Map (Map)

findUsages :: Project -> Set ExprHash -> Map ExprHash (Set StoreExpression)
findUsages = undefined