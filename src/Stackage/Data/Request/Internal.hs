-- | Types for stackage REST requests.
--
-- @since 0.1
module Stackage.Data.Request.Internal
  ( SnapshotReq (..),
    SnapshotIdReq (.., MkSnapshotIdReq),
  )
where

import Data.Text (Text)
import Servant.API (ToHttpApiData)

-- | Stackage snapshots.
--
-- @since 0.1
data SnapshotReq
  = -- | LTS snapshots e.g. @SnapshotReqLts (Just "20.14")@. 'Nothing'
    -- corresponds to the latest LTS.
    --
    -- @since 0.1
    UnsafeSnapshotReqLts (Maybe Text)
  | -- | Nightly snapshots e.g. @SnapshotReqNightly (Just "2023-03-14")@.
    -- 'Nothing' corresponds to the latest nightly.
    --
    -- @since 0.1
    UnsafeSnapshotReqNightly (Maybe Text)
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
newtype SnapshotIdReq = UnsafeSnapshotIdReq Text
  deriving stock
    ( -- | @since 0.1
      Eq
    )
  deriving
    ( -- | @since 0.1
      Show,
      -- | @since 0.1
      ToHttpApiData
    )
    via Text

-- | @since 0.1
pattern MkSnapshotIdReq :: Text -> SnapshotIdReq
pattern MkSnapshotIdReq t <- UnsafeSnapshotIdReq t

{-# COMPLETE MkSnapshotIdReq #-}
