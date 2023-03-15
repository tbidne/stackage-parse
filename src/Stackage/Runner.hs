-- | Runs stackage-parse with CLI args.
--
-- @since 0.1
module Stackage.Runner
  ( runStackageParser,
    withStackageParser,
  )
where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Asn
import Data.ByteString.Lazy.Char8 qualified as LChar8
import Data.Foldable (for_)
import Stackage
  ( PackageResp (..),
    Snapshot (..),
    StackageResp (..),
    getStackage,
  )
import Stackage.Args
  ( Args (..),
    Command (..),
    PkgListFormat (..),
    SnapshotStr (..),
    getArgs,
  )

-- | Parses CLI 'Args' and queries stackage. Results are printed.
--
-- @since 0.1
runStackageParser :: IO ()
runStackageParser = withStackageParser putStrLn

-- | Parses CLI 'Args' and queries stackage. Results are handled via the
-- argument fn.
--
-- @since 0.1
withStackageParser :: (String -> IO ()) -> IO ()
withStackageParser onStr = do
  args <- getArgs

  let snapshot = case args.nightlySnapshot of
        Just x -> strToSnapshot SnapshotNightly x
        Nothing -> case args.ltsSnapshot of
          Just x -> strToSnapshot SnapshotLts x
          -- default to latest nightly
          Nothing -> SnapshotNightly Nothing

  resp <- getStackage snapshot
  case args.command of
    Full -> onStr $ toJson resp
    GetSnapshot -> onStr $ toJson resp.snapshot
    ListPackages fmt -> do
      let fmtFn = case fmt of
            Just PkgListShort -> fmtShort
            Just PkgListCabal -> fmtCabal
            -- default to fmtShort
            Nothing -> fmtShort
      for_ resp.packages $ onStr . fmtFn
  where
    strToSnapshot cons SnapshotStrLatest = cons Nothing
    strToSnapshot cons (SnapshotStrLiteral l) = cons (Just l)

    fmtShort p = p.name ++ "-" ++ p.version
    fmtCabal p = p.name ++ " ==" ++ p.version

-- cabal run stackage-parse -- full | tail -n +2 | jq

toJson :: (ToJSON a) => a -> String
toJson = LChar8.unpack . Asn.encode
