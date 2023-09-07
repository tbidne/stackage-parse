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

import Control.Monad ((>=>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word8)
import Stackage.Data.Request.Internal
  ( SnapshotIdReq (MkSnapshotIdReq, UnsafeSnapshotIdReq),
    SnapshotReq (UnsafeSnapshotReqLts, UnsafeSnapshotReqNightly),
  )
import Text.Read qualified as TR

-- | @since 0.1
mkSnapshotReqLatestLts :: SnapshotReq
mkSnapshotReqLatestLts = UnsafeSnapshotReqLts Nothing

-- | @since 0.1
mkSnapshotReqLts :: Text -> Either Text SnapshotReq
mkSnapshotReqLts txt =
  -- Verify text has form XX.YY
  case T.split (== '.') txt of
    [x, y] | nonEmpty x && nonEmpty y ->
      case (readInt x, readInt y) of
        (Just _, Just _) -> Right (UnsafeSnapshotReqLts (Just txt))
        (Nothing, _) ->
          Left $
            "LTS version should be an integer, received " <> squote x
        (_, Nothing) ->
          Left $
            "LTS version should be an integer, received " <> squote y
    _ ->
      Left $
        "LTS snapshots have the form XX.YY, received " <> squote txt
  where
    readInt :: Text -> Maybe Word16
    readInt = TR.readMaybe . T.unpack

-- | @since 0.1
mkSnapshotReqLatestNightly :: SnapshotReq
mkSnapshotReqLatestNightly = UnsafeSnapshotReqNightly Nothing

-- | @since 0.1
mkSnapshotReqNightly :: Text -> Either Text SnapshotReq
mkSnapshotReqNightly txt = case T.split (== '-') txt of
  [y, m, d] | nonEmpty y && nonEmpty m && nonEmpty d ->
    case (readYear y, readMonth m, readDay d) of
      (Just _, Just _, Just _) -> Right (UnsafeSnapshotReqNightly (Just txt))
      (Nothing, _, _) ->
        Left $
          "Year should be an integer between 2000 and 2100, received " <> squote y
      (_, Nothing, _) ->
        Left $
          "Month should be an integer between 1 and 12, received " <> squote m
      (_, _, Nothing) ->
        Left $
          "Day should be an integer between 1 and 31, received " <> squote d
  _ ->
    Left $
      "Nightly snapshots have the form YYYY-MM-DD, received " <> squote txt
  where
    readYear = readDecimal @Word16 4 2000 2100
    readMonth = readDecimal @Word8 2 1 12
    readDay = readDecimal @Word8 2 1 31

    readDecimal :: (Ord a, Read a) => Int -> a -> a -> Text -> Maybe a
    readDecimal len l u =
      (\t -> if T.length t == len then Just t else Nothing)
        >=> TR.readMaybe . T.unpack
        >=> \n ->
          if n >= l && n <= u
            then Just n
            else Nothing

nonEmpty :: Text -> Bool
nonEmpty = not . T.null . T.strip

squote :: Text -> Text
squote t = "'" <> t <> "'"

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
mkSnapshotIdReq (UnsafeSnapshotReqLts (Just ltsStr)) = UnsafeSnapshotIdReq $ "lts-" <> ltsStr
mkSnapshotIdReq (UnsafeSnapshotReqLts Nothing) = UnsafeSnapshotIdReq "lts"
mkSnapshotIdReq (UnsafeSnapshotReqNightly (Just dateStr)) = UnsafeSnapshotIdReq $ "nightly-" <> dateStr
mkSnapshotIdReq (UnsafeSnapshotReqNightly Nothing) = UnsafeSnapshotIdReq "nightly"
