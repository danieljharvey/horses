{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.BuiltIns where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types

getBuiltInActual :: ForeignFunc -> App Variable
getBuiltInActual (NoArgs _ _) = do
  id' <- nextInt
  pure (BuiltInActual id' NoId)
getBuiltInActual _ = undefined

-- getBuiltInActual (OneArg _ _) = do

wrappedName :: Variable -> Variable
wrappedName = appendToVar "__unwrapped"

appendToVar :: Text -> Variable -> Variable
appendToVar tx (BuiltIn (Name n)) = BuiltIn (Name (n <> tx))
appendToVar _ a = a

wrappedVarName :: Variable -> Int -> Variable
wrappedVarName name i = appendToVar label name
  where
    label = "__unwrapped" <> T.pack (show i)

unwrap :: Variable -> Maybe Variable
unwrap (BuiltIn (Name n)) = BuiltIn . Name <$> T.stripSuffix "__unwrapped" n
unwrap a = Just a
