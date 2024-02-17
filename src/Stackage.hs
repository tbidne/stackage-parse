-- | @since 0.1
module Stackage
  ( -- * REST queries
    getLatestLts,
    getLatestNightly,
    getStackage,

    -- * REST request
    SnapshotReq,
    mkSnapshotReqLatestLts,
    mkSnapshotReqLts,
    mkSnapshotReqLatestNightly,
    mkSnapshotReqNightly,

    -- * REST response
    StackageResp (..),
    SnapshotResp (..),
    PackageResp (..),

    -- * Exceptions
    StackageException (..),
    ExceptionReason (..)
  )
where

import Control.Exception
  ( Exception (displayException),
    SomeException,
    throwIO,
  )
import Control.Monad (when)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error (UnicodeException)
import Network.HTTP.Client (Response)
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Status qualified as Status
import Stackage.Utils qualified as Utils
import Stackage.API
  ( withResponse,
  )
import Stackage.Data.Request
  ( SnapshotIdReq,
    SnapshotReq,
    mkSnapshotIdReq,
    mkSnapshotReqLatestLts,
    mkSnapshotReqLatestNightly,
    mkSnapshotReqLts,
    mkSnapshotReqNightly,
    unSnapshotIdReq,
  )
import Stackage.Data.Response
  ( PackageResp (MkPackageResp, name, origin, synopsis, version),
    SnapshotResp (MkSnapshotResp, compiler, created, ghc, name),
    StackageResp (MkStackageResp, packages, snapshot),
  )
import Text.JSON qualified as JSON

-- | Returns the 'StackageResp' for the latest nightly snapshot.
--
-- __Throws:__
--
--   * 'StackageException'
--
-- @since 0.1
getLatestNightly :: IO StackageResp
getLatestNightly = getStackage mkSnapshotReqLatestNightly

-- | Returns the 'StackageResp' for the latest LTS snapshot.
--
-- __Throws:__
--
--   * 'StackageException'
--
-- @since 0.1
getLatestLts :: IO StackageResp
getLatestLts = getStackage mkSnapshotReqLatestLts

-- | Returns the 'StackageResp' corresponding to the given snapshot.
--
-- __Throws:__
--
--   * 'StackageException'
--
-- @since 0.1
getStackage :: SnapshotReq -> IO StackageResp
getStackage snapshot = withResponse snapshotId $ \res -> do
  let bodyReader = HttpClient.responseBody res
      status = HttpClient.responseStatus res
      statusCode = getStatusCode res
      mkEx = MkStackageException snapshotId

  when (statusCode /= 200) $
    throwIO $
      mkEx (ReasonStatus status)

  bodyBs <-
    Utils.mapThrowLeft
      (mkEx . ReasonReadBody)
      =<< Utils.tryAny (mconcat <$> HttpClient.brConsume bodyReader)

  bodyTxt <-
    Utils.mapThrowLeft
      (mkEx . ReasonDecodeUtf8)
      (TEnc.decodeUtf8' bodyBs)

  let bodyStr = T.unpack bodyTxt

  Utils.mapThrowLeft
    (mkEx . ReasonDecodeJson bodyStr)
    (Utils.jsonResultToEither . JSON.decode $ bodyStr)
  where
    snapshotId = mkSnapshotIdReq snapshot

getStatusCode :: Response body -> Int
getStatusCode = Status.statusCode . HttpClient.responseStatus

data ExceptionReason
  = ReasonStatus Status
  | ReasonReadBody SomeException
  | ReasonDecodeUtf8 UnicodeException
  | ReasonDecodeJson String String
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | General network exception.
--
-- @since 0.1
data StackageException = MkStackageException
  { snapshotIdReq :: !SnapshotIdReq,
    reason :: !ExceptionReason
  }
  deriving stock
    ( -- | @since 0.1
      Show
    )

instance Exception StackageException where
  displayException ex =
    case ex.reason of
      ReasonStatus status ->
        if is404 status
          then
            mconcat
              [ "Received 404 for snapshot: ",
                snapshotIdTxt,
                ". Is the snapshot correct?"
              ]
          else
            mconcat
              [ "Received exception for snapshot: ",
                snapshotIdTxt
              ]
      ReasonReadBody readBodyEx ->
        mconcat
          [ "Exception when trying to read body for snapshot '",
            snapshotIdTxt,
            "':\n\n",
            displayException readBodyEx
          ]
      ReasonDecodeUtf8 decodeUtf8Ex ->
        mconcat
          [ "Exception decoding body to UTF-8 for snapshot '",
            snapshotIdTxt,
            "':\n\n",
            displayException decodeUtf8Ex
          ]
      ReasonDecodeJson jsonStr err ->
        mconcat
          [ "Could not decode JSON:\n\n",
            jsonStr,
            "\n\nError: ",
            err
          ]
    where
      snapshotIdTxt = T.unpack (unSnapshotIdReq ex.snapshotIdReq)
      is404 x = Status.statusCode x == 404