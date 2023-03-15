-- | @since 0.1
module Stackage
  ( -- * REST queries
    getLatestLts,
    getLatestNightly,
    getStackage,

    -- * Types

    -- ** REST API
    StackageResp (..),
    SnapshotResp (..),
    PackageResp (..),

    -- ** Other
    SnapshotReq (..),

    -- * Exceptions
    StackageException (..),
    StackageException404 (..),
  )
where

import Control.Exception (Exception (displayException), throwIO)
import Network.HTTP.Types.Status (Status (..))
import Servant.Client (ClientError (..))
import Servant.Client qualified as ServClient
import Stackage.API
  ( getStackageClientEnv,
    getStackageResp,
  )
import Stackage.Data.Request
  ( SnapshotReq (..),
    toSnapshotId,
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
getLatestNightly = getStackage (SnapshotReqNightly Nothing)

-- | Returns the 'StackageResp' for the latest LTS snapshot.
--
-- __Throws:__
--
--   * 'StackageException'
--   * 'StackageException404'
--
-- @since 0.1
getLatestLts :: IO StackageResp
getLatestLts = getStackage (SnapshotReqLts Nothing)

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
  cenv <- getStackageClientEnv (toSnapshotId snapshot)
  ServClient.runClientM getStackageResp cenv >>= \case
    Left err ->
      if is404 err
        then throwIO $ MkStackageException404 snapshot err
        else throwIO $ MkStackageException snapshot err
    Right str -> pure str
  where
    is404 (FailureResponse _ response) =
      response.responseStatusCode.statusCode == 404
    is404 _ = False

-- | Exception for 404s, likely due to wrong snapshot.
--
-- @since 0.1
data StackageException404 = MkStackageException404 SnapshotReq ClientError
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception StackageException404 where
  displayException (MkStackageException404 snapshot err) =
    mconcat
      [ "Received 404 for snapshot: ",
        toSnapshotId snapshot,
        ". Is that correct? Exception:\n\n",
        displayException err
      ]

-- | General network exception.
--
-- @since 0.1
data StackageException = MkStackageException SnapshotReq ClientError
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
        toSnapshotId snapshot,
        ". Exception:\n\n",
        displayException err
      ]
