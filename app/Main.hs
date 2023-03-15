-- | Main module.
--
-- @since 0.1
module Main (main) where

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

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  args <- getArgs

  let snapshot = case args.nightlySnapshot of
        Just x -> strToSnapshot SnapshotNightly x
        Nothing -> case args.ltsSnapshot of
          Just x -> strToSnapshot SnapshotLts x
          -- default to latest nightly
          Nothing -> SnapshotNightly Nothing

  resp <- getStackage snapshot
  case args.command of
    Full -> putStrLn $ toJson resp
    GetSnapshot -> putStrLn $ toJson resp.snapshot
    ListPackages fmt -> do
      let fmtFn = case fmt of
            Just PkgListShort -> fmtShort
            Just PkgListCabal -> fmtCabal
            -- default to fmtShort
            Nothing -> fmtShort
      for_ resp.packages $ putStrLn . fmtFn
  where
    strToSnapshot cons SnapshotStrLatest = cons Nothing
    strToSnapshot cons (SnapshotStrLiteral l) = cons (Just l)

    fmtShort p = p.name ++ "-" ++ p.version
    fmtCabal p = p.name ++ " ==" ++ p.version

-- cabal run stackage-parse -- full | tail -n +2 | jq

toJson :: (ToJSON a) => a -> String
toJson = LChar8.unpack . Asn.encode
