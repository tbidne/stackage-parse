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
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.HashSet qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncError
import Stackage
  ( PackageResp (..),
    SnapshotReq (..),
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
runStackageParser = withStackageParser (putStrLn . T.unpack)

-- | Parses CLI 'Args' and queries stackage. Results are handled via the
-- argument fn.
--
-- @since 0.1
withStackageParser :: (Text -> IO ()) -> IO ()
withStackageParser onStr = do
  args <- getArgs

  let snapshot = case args.nightlySnapshot of
        Just x -> strToSnapshot SnapshotReqNightly x
        Nothing -> case args.ltsSnapshot of
          Just x -> strToSnapshot SnapshotReqLts x
          -- default to latest nightly
          Nothing -> SnapshotReqNightly Nothing

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

    fmtShort p = p.name <> "-" <> p.version
    fmtCabal p = p.name <> " ==" <> p.version

    commaAppend p = p <> ","
    commaPrepend p = ", " <> p

getPackageExclusions :: FilePath -> IO (Text -> Bool)
getPackageExclusions path = do
  contents <- either throwIO pure . TEnc.decodeUtf8' =<< BS.readFile path

  let excludedSet =
        Set.fromList
          . fmap parsePkg
          . skipLines
          $ T.lines contents

  pure (`Set.member` excludedSet)
  where
    skipLines = filter (\l -> isComment l || isEmpty l)

    parsePkg line = case T.split (== '#') line of
      -- remove everything after the first #
      (p : _) -> T.strip p
      [] -> T.strip line

    -- Technically this is unnecessary as a "comment line" (e.g. # text ...)
    -- will never match a hackage package name. Still, seems better to
    -- strip them.
    isComment = not . T.isPrefixOf "#" . T.strip
    isEmpty = T.null . T.strip

toJson :: (ToJSON a) => a -> Text
toJson =
  TEnc.decodeUtf8With TEncError.lenientDecode
    . BSL.toStrict
    . Asn.encode
