{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Server.Helpers
  ( to400Error,
    to500Error,
    ErrorType (..),
    handleExceptT,
    handleEither,
    handleServerM,
  )
where

import Control.Monad.Except
import Data.Bifunctor
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Mimsa.Printer
import Servant
import Server.ServerConfig
import Server.ServerM

data ErrorType
  = UserError
  | InternalError

handleServerM :: (Printer e) => ServerConfig -> ErrorType -> ServerM e a -> Handler a
handleServerM cfg et computation = do
  let ioEither = runServerM cfg computation
  let f =
        first
          ( \e -> case et of
              UserError -> to400Error e
              InternalError -> to500Error e
          )
  Handler $ ExceptT $ f <$> ioEither

handleExceptT :: Printer e => ErrorType -> ExceptT e IO a -> Handler a
handleExceptT et computation =
  let errorFunc = case et of
        UserError -> to400Error
        InternalError -> to500Error
   in Handler $ withExceptT errorFunc computation

handleEither :: Printer e => ErrorType -> Either e a -> Handler a
handleEither et computation =
  let errorFunc = case et of
        UserError -> to400Error
        InternalError -> to500Error
   in case computation of
        Right a -> pure a
        Left e -> throwError (errorFunc e)

to400Error :: (Printer a) => a -> ServerError
to400Error a = err400 {errBody = buildMsg a}
  where
    buildMsg = encodeUtf8 . fromStrict . prettyPrint

to500Error :: (Printer a) => a -> ServerError
to500Error a = err500 {errBody = buildMsg a}
  where
    buildMsg = encodeUtf8 . fromStrict . prettyPrint
