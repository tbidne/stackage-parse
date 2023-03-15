-- | Types for stackage snapshots.
--
-- @since 0.1
module Stackage.Data.Snapshot
  ( Snapshot (..),
    toSnapshotId,
  )
where

-- | Stackage snapshots.
--
-- @since 0.1
data Snapshot
  = -- | LTS snapshots e.g. @SnapshotLts (Just "20.14")@. 'Nothing' corresponds to
    -- the latest LTS.
    --
    -- @since 0.1
    SnapshotLts (Maybe String)
  | -- | Nightly snapshots e.g. @SnapshotNightly (Just "2023-03-14")@. 'Nothing'
    -- corresponds to the latest nightly.
    --
    -- @since 0.1
    SnapshotNightly (Maybe String)
  deriving stock (Eq, Ord, Show)

-- | Turns a snapshot into a string suitable for use with a stackage request
-- i.e @stackage.org\/<toSnapshotId snapshot>@.
--
-- ==== __Examples__
-- >>> toSnapshotId (SnapshotLts (Just "20.14"))
-- "lts-20.14"
--
-- >>> toSnapshotId (SnapshotLts Nothing)
-- "lts"
--
-- >>> toSnapshotId (SnapshotNightly (Just "2023-03-14"))
-- "nightly-2023-03-14"
--
-- >>> toSnapshotId (SnapshotNightly Nothing)
-- "nightly"
--
-- @since 0.1
toSnapshotId :: Snapshot -> String
toSnapshotId (SnapshotLts (Just ltsStr)) = "lts-" ++ ltsStr
toSnapshotId (SnapshotLts Nothing) = "lts"
toSnapshotId (SnapshotNightly (Just dateStr)) = "nightly-" ++ dateStr
toSnapshotId (SnapshotNightly Nothing) = "nightly"
