-- | Types for stackage REST requests.
--
-- @since 0.1
module Stackage.Data.Request
  ( SnapshotReq (..),
    toSnapshotId,
  )
where

import Data.Text (Text)

-- | Stackage snapshots.
--
-- @since 0.1
data SnapshotReq
  = -- | LTS snapshots e.g. @SnapshotReqLts (Just "20.14")@. 'Nothing' corresponds to
    -- the latest LTS.
    --
    -- @since 0.1
    SnapshotReqLts (Maybe Text)
  | -- | Nightly snapshots e.g. @SnapshotReqNightly (Just "2023-03-14")@. 'Nothing'
    -- corresponds to the latest nightly.
    --
    -- @since 0.1
    SnapshotReqNightly (Maybe Text)
  deriving stock (Eq, Ord, Show)

-- | Turns a snapshot into a string suitable for use with a stackage request
-- i.e @stackage.org\/<toSnapshotId snapshot>@.
--
-- ==== __Examples__
-- >>> toSnapshotId (SnapshotReqLts (Just "20.14"))
-- "lts-20.14"
--
-- >>> toSnapshotId (SnapshotReqLts Nothing)
-- "lts"
--
-- >>> toSnapshotId (SnapshotReqNightly (Just "2023-03-14"))
-- "nightly-2023-03-14"
--
-- >>> toSnapshotId (SnapshotReqNightly Nothing)
-- "nightly"
--
-- @since 0.1
toSnapshotId :: SnapshotReq -> Text
toSnapshotId (SnapshotReqLts (Just ltsStr)) = "lts-" <> ltsStr
toSnapshotId (SnapshotReqLts Nothing) = "lts"
toSnapshotId (SnapshotReqNightly (Just dateStr)) = "nightly-" <> dateStr
toSnapshotId (SnapshotReqNightly Nothing) = "nightly"
