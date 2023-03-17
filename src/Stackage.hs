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
    StackageException404 (..),
  )
where

import Control.Exception (Exception (displayException), throwIO)
import Data.Text qualified as T
import Network.HTTP.Types.Status (Status (..))
import Servant.Client (ClientError (..))
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
  ( PackageResp (..),
    SnapshotResp (..),
    StackageResp (..),
  )

-- | Returns the 'StackageResp' for the latest nightly snapshot.
--
-- __Throws:__
--
--   * 'StackageException'
--   * 'StackageException404'
--
-- @since 0.1
getLatestNightly :: IO StackageResp
getLatestNightly = getStackage mkSnapshotReqLatestNightly

-- | Returns the 'StackageResp' for the latest LTS snapshot.
--
-- __Throws:__
--
--   * 'StackageException'
--   * 'StackageException404'
--
-- @since 0.1
getLatestLts :: IO StackageResp
getLatestLts = getStackage mkSnapshotReqLatestLts

-- | Returns the 'StackageResp' corresponding to the given snapshot.
--
-- __Throws:__
--
--   * 'StackageException'
--   * 'StackageException404'
--
-- @since 0.1
getStackage :: SnapshotReq -> IO StackageResp
getStackage snapshot = do
  cenv <- getStackageClientEnv
  ServClient.runClientM (getStackageResp snapshotId) cenv >>= \case
    Left err ->
      if is404 err
        then throwIO $ MkStackageException404 snapshotId err
        else throwIO $ MkStackageException snapshotId err
    Right str -> pure str
  where
    snapshotId = mkSnapshotIdReq snapshot

    is404 (FailureResponse _ response) =
      response.responseStatusCode.statusCode == 404
    is404 _ = False

-- | Exception for 404s, likely due to wrong snapshot.
--
-- @since 0.1
data StackageException404 = MkStackageException404 SnapshotIdReq ClientError
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception StackageException404 where
  displayException (MkStackageException404 snapshotId err) =
    mconcat
      [ "Received 404 for snapshot: ",
        T.unpack (unSnapshotIdReq snapshotId),
        ". Is that correct? Exception:\n\n",
        displayException err
      ]

-- | General network exception.
--
-- @since 0.1
data StackageException = MkStackageException SnapshotIdReq ClientError
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception StackageException where
  displayException (MkStackageException snapshot err) =
    mconcat
      [ "Received exception for snapshot: ",
        T.unpack (unSnapshotIdReq snapshot),
        ". Exception:\n\n",
        displayException err
      ]
