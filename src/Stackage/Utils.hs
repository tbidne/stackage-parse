module Stackage.Utils
  ( -- * Exceptions
    try,
    tryAny,
    mapThrowLeft,

    -- * JSON
    jsonResultToEither,
    readJSONObj,
  )
where

import Control.Exception
  ( Exception (fromException, toException),
    SomeAsyncException (SomeAsyncException),
    SomeException,
    throwIO,
  )
import Control.Exception qualified as Ex
import Data.Bifunctor (first)
import Text.JSON (JSObject, JSValue (JSObject), Result (Error, Ok))

mapThrowLeft :: (Exception e2) => (e1 -> e2) -> Either e1 a -> IO a
mapThrowLeft f = throwLeft . first f

throwLeft :: (Exception e) => Either e a -> IO a
throwLeft (Right x) = pure x
throwLeft (Left e) = throwIO e

jsonResultToEither :: Result a -> Either String a
jsonResultToEither (Error e) = Left e
jsonResultToEither (Ok x) = Right x

readJSONObj :: (JSObject JSValue -> Result a) -> JSValue -> Result a
readJSONObj onObj (JSObject obj) = onObj obj
readJSONObj _ other = Error $ "Expected json obj, received: " ++ show other

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try @SomeException

try :: (Exception e) => IO a -> IO (Either e a)
try io =
  Ex.try io >>= \case
    Left ex
      | isSyncException ex -> pure $ Left ex
      | otherwise -> throwIO ex
    Right x -> pure $ Right x

isSyncException :: (Exception e) => e -> Bool
isSyncException e =
  case fromException (toException e) of
    Just (SomeAsyncException _) -> False
    Nothing -> True
