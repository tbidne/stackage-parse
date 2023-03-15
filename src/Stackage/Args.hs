-- | Provides CLI args.
--
-- @since 0.1
module Stackage.Args
  ( getArgs,
    Args (..),
    SnapshotStr (..),
    Command (..),
    PkgListFormat (..),
  )
where

import Control.Applicative qualified as A
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.String (IsString)
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
    ltsSnapshot :: !(Maybe SnapshotStr),
    -- | @since 0.1
    nightlySnapshot :: !(Maybe SnapshotStr),
    -- | @since 0.1
    command :: !Command
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
data SnapshotStr
  = -- | @since 0.1
    SnapshotStrLatest
  | -- | @since 0.1
    SnapshotStrLiteral !String
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

-- | Command.
--
-- @since 0.1
data Command
  = -- | @since 0.1
    Full
  | -- | @since 0.1
    ListPackages !(Maybe PkgListFormat)
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
    <*> commandParser
    <**> OA.helper
    <**> version

ltsSnapshotParser :: Parser (Maybe SnapshotStr)
ltsSnapshotParser = snapshotParser' options
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

nightlySnapshotParser :: Parser (Maybe SnapshotStr)
nightlySnapshotParser = snapshotParser' options
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

snapshotParser' :: Mod OptionFields SnapshotStr -> Parser (Maybe SnapshotStr)
snapshotParser' = A.optional . OA.option readSnapshot
  where
    readSnapshot =
      OA.str <&> \case
        "latest" -> SnapshotStrLatest
        other -> SnapshotStrLiteral other

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

    listPackagesParser = ListPackages <$> pkgListFormatParser
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
