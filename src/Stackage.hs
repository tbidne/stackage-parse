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
  )
where

import Control.Exception (Exception (displayException), throwIO)
import Data.Text qualified as T
import Network.HTTP.Types.Status (Status (statusCode))
import Servant.Client (ClientError (FailureResponse))
import Servant.Client qualified as ServClient
import Stackage.API
  ( getStackageClientEnv,
    getStackageResp,
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
getStackage snapshot = do
  cenv <- getStackageClientEnv
  ServClient.runClientM (getStackageResp snapshotId) cenv >>= \case
    Left err -> throwIO $ MkStackageException snapshotId err
    Right str -> pure str
  where
    snapshotId = mkSnapshotIdReq snapshot

-- | General network exception.
--
-- @since 0.1
data StackageException = MkStackageException !SnapshotIdReq !ClientError
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception StackageException where
  displayException (MkStackageException snapshot err) =
    msg <> "Exception:\n\n" <> displayException err
    where
      msg
        -- Slightly more specific error for 404s since this is likely to be
        -- the most common error.
        | is404 =
            mconcat
              [ "Received 404 for snapshot: ",
                T.unpack (unSnapshotIdReq snapshot),
                ". Is the snapshot correct? "
              ]
        | otherwise =
            mconcat
              [ "Received exception for snapshot: ",
                T.unpack (unSnapshotIdReq snapshot),
                "."
              ]
      is404 = case err of
        (FailureResponse _ response) ->
          response.responseStatusCode.statusCode == 404
        _ -> False
