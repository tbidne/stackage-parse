-- | Runs stackage-parse with CLI args.
--
-- @since 0.1
module Stackage.Runner
  ( runStackageParser,
    withStackageParser,
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Aeson (ToJSON)
import Data.Aeson qualified as Asn
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LChar8
import Data.Foldable (for_)
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Stackage
  ( PackageResp (..),
    Snapshot (..),
    StackageResp (..),
    getStackage,
  )
import Stackage.Args
  ( Args (..),
    Comma (..),
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

  excludeFn <- case args.excludeFile of
    Nothing -> pure (const False)
    Just f -> getPackageExclusions f

  resp <- getStackage snapshot
  case args.command of
    Full -> onStr $ do
      let packages = filter (not . excludeFn . (.name)) resp.packages
      toJson (resp {packages})
    GetSnapshot -> onStr $ toJson resp.snapshot
    ListPackages fmt comma -> do
      let fmtFn = case fmt of
            Just PkgListShort -> fmtShort
            Just PkgListCabal -> fmtCabal
            -- default to fmtShort
            Nothing -> fmtShort
          commaFn = case comma of
            Just CommaAppend -> commaAppend
            Just CommaPrepend -> commaPrepend
            Nothing -> id
      for_ resp.packages $ \pkg ->
        unless (excludeFn pkg.name) (onStr . commaFn . fmtFn $ pkg)
  where
    strToSnapshot cons SnapshotStrLatest = cons Nothing
    strToSnapshot cons (SnapshotStrLiteral l) = cons (Just l)

    fmtShort p = p.name ++ "-" ++ p.version
    fmtCabal p = p.name ++ " ==" ++ p.version

    commaAppend p = p ++ ","
    commaPrepend p = ", " ++ p

getPackageExclusions :: FilePath -> IO (String -> Bool)
getPackageExclusions path = do
  contents <-
    BS.readFile path
      >>= ( \case
              Left err -> throwIO err
              Right c -> pure c
          )
        . TEnc.decodeUtf8'

  let excludedSet = Set.fromList . skipComments $ T.lines contents

  pure $ (`Set.member` excludedSet) . T.pack
  where
    -- Technically this is unnecessary as a "comment line" (e.g. # text ...)
    -- will never match a hackage package name. Still, seems better to
    -- strip them.
    skipComments = filter (not . T.isPrefixOf "#")

toJson :: (ToJSON a) => a -> String
toJson = LChar8.unpack . Asn.encode
