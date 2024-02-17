-- | Runs stackage-parse with CLI args.
--
-- @since 0.1
module Stackage.Runner
  ( runStackageParser,
    withStackageParser,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Stackage
  ( PackageResp (name, version),
    StackageResp (packages, snapshot),
    getStackage,
    mkSnapshotReqLatestNightly,
  )
import Stackage.Args
  ( Args (command, excludeFile, includeFile, ltsSnapshot, nightlySnapshot),
    Comma (CommaAppend, CommaPrepend),
    Command (Full, GetSnapshot, ListPackages),
    PkgListFormat (PkgListCabal, PkgListShort),
    getArgs,
  )
import Text.JSON (JSON)
import Text.JSON qualified as JSON

-- | Parses CLI 'Args' and queries stackage. Results are printed.
--
-- @since 0.1
runStackageParser :: IO ()
runStackageParser = withStackageParser (putStrLn . T.unpack)

-- | Parses CLI 'Args' and queries stackage. Results are handled via the
-- argument fn.
--
-- @since 0.1
withStackageParser :: (Text -> IO ()) -> IO ()
withStackageParser onStr = do
  args <- getArgs

  -- default to latest nightly
  let snapshot =
        fromMaybe
          mkSnapshotReqLatestNightly
          (args.nightlySnapshot <|> args.ltsSnapshot)

  filterFn <- case (args.includeFile, args.excludeFile) of
    (Just f, Just g) -> do
      f' <- getIncludeFn f
      g' <- getExcludeFn g
      pure $ \x -> f' x && g' x
    (Just f, _) -> getIncludeFn f
    (_, Just g) -> getExcludeFn g
    (Nothing, Nothing) -> pure (const True)

  resp <- getStackage snapshot
  case args.command of
    Full -> onStr $ do
      let packages = filter (filterFn . (.name)) resp.packages
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
        when (filterFn pkg.name) (onStr . commaFn . fmtFn $ pkg)
  where
    fmtShort p = p.name <> "-" <> p.version
    fmtCabal p = p.name <> " ==" <> p.version

    commaAppend p = p <> ","
    commaPrepend p = ", " <> p

getExcludeFn :: FilePath -> IO (Text -> Bool)
getExcludeFn path = (\s -> not . (`Set.member` s)) <$> parsePackageFileList path

getIncludeFn :: FilePath -> IO (Text -> Bool)
getIncludeFn path = flip Set.member <$> parsePackageFileList path

parsePackageFileList :: FilePath -> IO (HashSet Text)
parsePackageFileList path = do
  contents <- either throwIO pure . TEnc.decodeUtf8' =<< BS.readFile path

  pure
    $ Set.fromList
      . fmap parsePkg
      . skipLines
    $ T.lines contents
  where
    skipLines = filter (\l -> nonComment l && nonEmpty l)

    parsePkg line = case T.split (== '#') line of
      -- remove everything after the first #
      (p : _) -> T.strip p
      [] -> T.strip line

    -- Technically this is unnecessary as a "comment line" (e.g. # text ...)
    -- will never match a hackage package name. Still, seems better to
    -- strip them.
    nonComment = not . T.isPrefixOf "#" . T.strip
    nonEmpty = not . T.null . T.strip

toJson :: (JSON a) => a -> Text
toJson = T.pack . JSON.encode
