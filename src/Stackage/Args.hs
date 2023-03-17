-- | Provides CLI args.
--
-- @since 0.1
module Stackage.Args
  ( getArgs,
    Args (..),
    Command (..),
    PkgListFormat (..),
    Comma (..),
  )
where

import Control.Applicative qualified as A
import Data.List qualified as L
import Data.String (IsString)
import Data.Text (Text)
import Options.Applicative
  ( CommandFields,
    InfoMod,
    Mod,
    OptionFields,
    Parser,
    ParserInfo (..),
    (<**>),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))
import Stackage.Data.Request
  ( SnapshotReq,
    mkSnapshotReqLatestLts,
    mkSnapshotReqLatestNightly,
    mkSnapshotReqLts,
    mkSnapshotReqNightly,
  )

-- | Retrieves CLI 'Args'.
--
-- @since 0.1
getArgs :: IO Args
getArgs = OA.execParser parserInfoArgs

-- | 'ParserInfo' type for parsing 'Args'.
--
-- @since 0.1
parserInfoArgs :: ParserInfo Args
parserInfoArgs =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = Chunk desc,
      infoHeader = Chunk headerTxt,
      infoFooter = Chunk footerTxt,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    headerTxt = Just "stackage-parse: A tool for parsing stackage snapshot data."
    footerTxt = Just versNum
    desc = Nothing

-- | CLI args.
--
-- @since 0.1
data Args = MkArgs
  { -- | @since 0.1
    ltsSnapshot :: !(Maybe SnapshotReq),
    -- | @since 0.1
    nightlySnapshot :: !(Maybe SnapshotReq),
    -- | @since 0.1
    excludeFile :: !(Maybe FilePath),
    -- | @since 0.1
    command :: !Command
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | Format for package list.
--
-- @since 0.1
data PkgListFormat
  = -- | @since 0.1
    PkgListShort
  | -- | @since 0.1
    PkgListCabal
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | Appends/Prepends a comma to the package list.
--
-- @since 0.1
data Comma
  = -- | @since 0.1
    CommaAppend
  | -- | @since 0.1
    CommaPrepend
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | Command.
--
-- @since 0.1
data Command
  = -- | @since 0.1
    Full
  | -- | @since 0.1
    ListPackages !(Maybe PkgListFormat) !(Maybe Comma)
  | -- | @since 0.1
    GetSnapshot
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

argsParser :: Parser Args
argsParser =
  MkArgs
    <$> ltsSnapshotParser
    <*> nightlySnapshotParser
    <*> excludeFileParser
    <*> commandParser
    <**> OA.helper
    <**> version

ltsSnapshotParser :: Parser (Maybe SnapshotReq)
ltsSnapshotParser =
  snapshotParser' mkSnapshotReqLatestLts mkSnapshotReqLts options
  where
    options =
      mconcat
        [ OA.long "lts",
          OA.metavar "(latest|LTS_STR)",
          OA.help helpTxt
        ]
    helpTxt =
      mconcat
        [ "LTS snapshot e.g. 20.14 or the string 'latest'. ",
          "Overridden by --nightly."
        ]

nightlySnapshotParser :: Parser (Maybe SnapshotReq)
nightlySnapshotParser =
  snapshotParser' mkSnapshotReqLatestNightly mkSnapshotReqNightly options
  where
    options =
      mconcat
        [ OA.long "nightly",
          OA.metavar "(latest|DATE_STR)",
          OA.help helpTxt
        ]
    helpTxt =
      mconcat
        [ "Nightly snapshot e.g. 2023-03-14 or the string 'latest'. ",
          "Overrides --lts."
        ]

excludeFileParser :: Parser (Maybe FilePath)
excludeFileParser =
  A.optional $
    OA.option OA.str $
      mconcat
        [ OA.long "exclude",
          OA.short 'e',
          OA.metavar "PATH",
          OA.help helpTxt
        ]
  where
    helpTxt =
      mconcat
        [ "Path to file with a list of packages to exclude from the package ",
          "list. Each package should be listed on a separate line, without ",
          "version numbers."
        ]

snapshotParser' ::
  -- | Constructor for latest snapshot.
  SnapshotReq ->
  -- | Constructor for specific snapshot. Can fail.
  (Text -> Maybe SnapshotReq) ->
  -- | Options.
  Mod OptionFields SnapshotReq ->
  Parser (Maybe SnapshotReq)
snapshotParser' mkLatest mk = A.optional . OA.option readSnapshot
  where
    readSnapshot =
      OA.str >>= \case
        "latest" -> pure mkLatest
        other -> case mk other of
          Just s -> pure s
          Nothing ->
            fail $
              mconcat
                [ "Bad snapshot format. LTS snapshots should have the form ",
                  "XX.YY, while nightly snapshots are YYYY-MM-DD."
                ]

commandParser :: Parser Command
commandParser =
  OA.hsubparser $
    mconcat
      [ mkCommand "full" fullParser fullHelp,
        mkCommand "pkgs" listPackagesParser listPackagesHelp,
        mkCommand "snapshot" getSnapshotParser getSnapshotHelp
      ]
  where
    fullParser = pure Full
    fullHelp =
      OA.progDesc
        "Prints full package list and snapshot metadata formatted as json."

    listPackagesParser = ListPackages <$> pkgListFormatParser <*> commaParser
    listPackagesHelp = OA.progDesc "Lists all packages in a given snapshot."

    getSnapshotParser = pure GetSnapshot
    getSnapshotHelp = OA.progDesc "Prints snapshot metadata formatted as json."

pkgListFormatParser :: Parser (Maybe PkgListFormat)
pkgListFormatParser =
  A.optional $
    OA.option readListFormat $
      mconcat
        [ OA.long "format",
          OA.short 'f',
          OA.metavar "(short|cabal)",
          OA.help helpTxt
        ]
  where
    readListFormat =
      OA.str @String >>= \case
        "short" -> pure PkgListShort
        "cabal" -> pure PkgListCabal
        other -> fail $ "Unknown format: " ++ other
    helpTxt =
      mconcat
        [ "Short corresponds to <pkg-name>-<vers> e.g. 'text-2.0.1'.",
          "Cabal corresponds to format suitable to be pasted into a cabal ",
          "file's 'build-depends' e.g. 'text ==2.0.1'. Defaults to short."
        ]

commaParser :: Parser (Maybe Comma)
commaParser =
  A.optional $
    OA.option readComma $
      mconcat
        [ OA.long "comma",
          OA.short 'c',
          OA.metavar "(append|prepend)",
          OA.help helpTxt
        ]
  where
    readComma =
      OA.str @String >>= \case
        "append" -> pure CommaAppend
        "prepend" -> pure CommaPrepend
        other -> fail $ "Unknown comma: " ++ other
    helpTxt = "If given, prepends/appends a comma before/after each entry."

mkCommand :: String -> Parser a -> InfoMod a -> Mod CommandFields a
mkCommand cmdTxt parser helpTxt = OA.command cmdTxt (OA.info parser helpTxt)

version :: Parser (a -> a)
version = OA.infoOption txt (OA.long "version" <> OA.short 'v')
  where
    txt =
      L.intercalate
        "\n"
        [ "stackage-parse",
          versNum
        ]

versNum :: (IsString a) => a
versNum = "Version: 0.1"
