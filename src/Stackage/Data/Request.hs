-- | Types for stackage REST requests.
--
-- @since 0.1
module Stackage.Data.Request
  ( SnapshotReq,
    mkSnapshotReqLatestLts,
    mkSnapshotReqLts,
    mkSnapshotReqLatestNightly,
    mkSnapshotReqNightly,
    SnapshotIdReq (MkSnapshotIdReq),
    unSnapshotIdReq,
    mkSnapshotIdReq,
  )
where

import Data.Text (Text)
import Servant.API (ToHttpApiData)

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
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
mkSnapshotReqLatestLts :: SnapshotReq
mkSnapshotReqLatestLts = SnapshotReqLts Nothing

-- | @since 0.1
mkSnapshotReqLts :: Text -> Maybe SnapshotReq
mkSnapshotReqLts = Just . SnapshotReqLts . Just

-- | @since 0.1
mkSnapshotReqLatestNightly :: SnapshotReq
mkSnapshotReqLatestNightly = SnapshotReqLts Nothing

-- | @since 0.1
mkSnapshotReqNightly :: Text -> Maybe SnapshotReq
mkSnapshotReqNightly = Just . SnapshotReqNightly . Just

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

-- | @since 0.1
unSnapshotIdReq :: SnapshotIdReq -> Text
unSnapshotIdReq (UnsafeSnapshotIdReq t) = t

-- | Turns a snapshot into a string suitable for use with a stackage request
-- i.e @stackage.org\/<mkSnapshotIdReq snapshot>@.
--
-- ==== __Examples__
-- >>> mkSnapshotIdReq (SnapshotReqLts (Just "20.14"))
-- "lts-20.14"
--
-- >>> mkSnapshotIdReq (SnapshotReqLts Nothing)
-- "lts"
--
-- >>> mkSnapshotIdReq (SnapshotReqNightly (Just "2023-03-14"))
-- "nightly-2023-03-14"
--
-- >>> mkSnapshotIdReq (SnapshotReqNightly Nothing)
-- "nightly"
--
-- @since 0.1
mkSnapshotIdReq :: SnapshotReq -> SnapshotIdReq
mkSnapshotIdReq (SnapshotReqLts (Just ltsStr)) = UnsafeSnapshotIdReq $ "lts-" <> ltsStr
mkSnapshotIdReq (SnapshotReqLts Nothing) = UnsafeSnapshotIdReq "lts"
mkSnapshotIdReq (SnapshotReqNightly (Just dateStr)) = UnsafeSnapshotIdReq $ "nightly-" <> dateStr
mkSnapshotIdReq (SnapshotReqNightly Nothing) = UnsafeSnapshotIdReq "nightly"
