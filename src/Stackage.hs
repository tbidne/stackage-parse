-- | @since 0.1
module Stackage
  ( -- * Types

    -- ** REST API
    StackageResp (..),
    SnapshotResp (..),
    PackageResp (..),

    -- ** Other
    Snapshot (..),

    -- * Functions
    getLatestLts,
    getLatestNightly,
    getStackage,
  )
where

import Control.Exception (throwIO)
import Servant.Client qualified as ServClient
import Stackage.API
  ( getStackageClientEnv,
    getStackageResp,
  )
import Stackage.Data.Response
  ( PackageResp (..),
    SnapshotResp (..),
    StackageResp (..),
  )
import Stackage.Data.Snapshot
  ( Snapshot (..),
    toSnapshotId,
  )

-- | Returns the 'StackageResp' for the latest nightly snapshot.
--
-- @since 0.1
getLatestNightly :: IO StackageResp
getLatestNightly = getStackage (SnapshotNightly Nothing)

-- | Returns the 'StackageResp' for the latest LTS snapshot.
--
-- @since 0.1
getLatestLts :: IO StackageResp
getLatestLts = getStackage (SnapshotLts Nothing)

-- | Returns the 'StackageResp' corresponding to the given snapshot.
--
-- @since 0.1
getStackage :: Snapshot -> IO StackageResp
getStackage snapshot = do
  cenv <- getStackageClientEnv (toSnapshotId snapshot)
  ServClient.runClientM getStackageResp cenv >>= \case
    Left err -> throwIO err
    Right str -> pure str
