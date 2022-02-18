module Language.Mimsa.Transform.Inliner where

import Language.Mimsa.Types.AST

shouldInline :: Expr var ann -> Bool
shouldInline (MyLiteral _ _) = True
shouldInline (MyArray _ as) = and (shouldInline <$> as)
shouldInline (MyRecord _ as) = and (shouldInline <$> as)
shouldInline _ = False
