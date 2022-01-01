{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Server.MimsaHandler
  ( runMimsaHandlerT,
    throwMimsaHandlerError,
    MimsaHandlerT (..),
    JsonPost,
    MimsaHandler,
    throwMimsaError,
    returnMimsa,
    lift,
  )
where

import Control.Monad.Except
import Language.Mimsa.Printer
import Language.Mimsa.Types.Error
import Servant
import Server.Errors.UserErrorResponse

-- magic version of handler that allows throwing of custom errors
-- from https://github.com/haskell-servant/servant/blob/c1105899f47d5537c1bdc8c86bee3523bdf05282/doc/cookbook/uverb/UVerb.lhs

newtype MimsaHandlerT xs m a = MimsaHandlerT {unMimsaHandlerT :: ExceptT (Union xs) m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

-- | Deliberately hide 'ExceptT's 'MonadError' instance to be able to use
-- underlying monad's instance.
instance MonadError e m => MonadError e (MimsaHandlerT xs m) where
  throwError = lift . throwError
  catchError (MimsaHandlerT act) h =
    MimsaHandlerT $
      ExceptT $
        runExceptT act `catchError` (runExceptT . unMimsaHandlerT . h)

-- | This combinator runs 'MimsaHandlerT'. It applies 'respond' internally, so the handler
-- may use the usual 'return'.
runMimsaHandlerT :: (Monad m, HasStatus x, IsMember x xs) => MimsaHandlerT xs m x -> m (Union xs)
runMimsaHandlerT (MimsaHandlerT act) = either id id <$> runExceptT (act >>= respond)

-- | Short-circuit 'MimsaHandlerT' computation returning one of the response types.
throwMimsaHandlerError :: (Monad m, HasStatus x, IsMember x xs) => x -> MimsaHandlerT xs m a
throwMimsaHandlerError = MimsaHandlerT . ExceptT . fmap Left . respond

throwMimsaError ::
  (Show ann, Printer ann, Monad m) =>
  Error ann ->
  MimsaHandlerT (ReturnRow res) m a
throwMimsaError =
  throwMimsaHandlerError . WithStatus @400 . toUserError

-- | Return a value in our fancy monad
returnMimsa ::
  (Monad m) =>
  a ->
  MimsaHandlerT (ReturnRow a) m (WithStatus 200 a)
returnMimsa a = pure (WithStatus @200 a)

type JsonPost res =
  UVerb
    'POST
    '[JSON]
    '[ WithStatus 200 res,
       WithStatus 400 UserErrorResponse
     ]

type ReturnRow res =
  '[ WithStatus 200 res,
     WithStatus 400 UserErrorResponse
   ]

type MimsaHandler res =
  Handler
    ( Union
        '[ WithStatus 200 res,
           WithStatus 400 UserErrorResponse
         ]
    )
