module Main (main) where

import Control.Exception (Exception (displayException), try)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Asn
import Data.Foldable (traverse_)
import Data.HashSet qualified as Set
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Version.Package qualified as PV
import Stackage (StackageException (MkStackageException))
import Stackage.Data.Response
  ( SnapshotResp (MkSnapshotResp, compiler, created, ghc, name),
    StackageResp (snapshot),
  )
import Stackage.Runner (withStackageParser)
import System.Environment (withArgs)
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))

main :: IO ()
main = guardOrElse' "RUN_FUNC" ExpectEnvSet runTests warn
  where
    runTests = Tasty.defaultMain tests
    warn = putStrLn "*** Functional tests disabled. Enable with RUN_FUNC=1. ***"

tests :: TestTree
tests = do
  Tasty.testGroup
    "Functional Tests"
    [ pkgsTests,
      fullTests,
      testSnapshot,
      testExclude,
      testInclude,
      testIncludeExclude,
      test404
    ]

pkgsTests :: TestTree
pkgsTests =
  Tasty.testGroup
    "Pkgs command"
    [ testPkgsFormatShort,
      testPkgsFormatCabal,
      testPkgsFormatNone
    ]

testPkgsFormatCabal :: TestTree
testPkgsFormatCabal = testCase "--format cabal" $ do
  results <- run pkgsArgs

  case results of
    [] -> assertFailure "Received empty pkgs list"
    pkgs@(_ : _) -> traverse_ validatePkg pkgs
  where
    pkgsArgs = ["pkgs", "-f", "cabal"]
    validatePkg p = do
      versStr <- case T.split (== ' ') p of
        [] -> assertFailure "Received empty package name"
        xs@(_ : _) ->
          let last' = last xs
           in case T.stripPrefix "==" last' of
                Nothing ->
                  assertFailure $
                    "Version string did not match expected format '==*': "
                      <> T.unpack last'
                Just s -> pure s
      case PV.fromText versStr of
        Left err ->
          assertFailure $ "Malformed version string: " <> displayException err
        Right _ -> pure ()

testPkgsFormatShort :: TestTree
testPkgsFormatShort = testCase "--format short" (runsPkgsFormatShort args)
  where
    args = ["--format", "short"]

testPkgsFormatNone :: TestTree
testPkgsFormatNone = testCase "No format" (runsPkgsFormatShort [])

runsPkgsFormatShort :: [String] -> IO ()
runsPkgsFormatShort fmtArgs = do
  results <- run pkgsArgs

  case results of
    [] -> assertFailure "Received empty pkgs list"
    pkgs@(_ : _) -> traverse_ validatePkg pkgs
  where
    pkgsArgs = "pkgs" : fmtArgs
    validatePkg p = do
      versStr <- case T.split (== '-') p of
        [] -> assertFailure "Received empty package name"
        xs@(_ : _) -> pure $ last xs
      case PV.fromText versStr of
        Left err ->
          assertFailure $ "Malformed version string: " <> displayException err
        Right _ -> pure ()

fullTests :: TestTree
fullTests =
  Tasty.testGroup
    "Full command"
    [ testFullNightlyOverridesLts,
      testFullNightlyLatest,
      testFullNightly,
      testFullLtsLatest,
      testFullLts,
      testFull
    ]

testFullNightlyLatest :: TestTree
testFullNightlyLatest =
  testCase "--nightly latest" $ runsFull args (const $ pure ())
  where
    args = ["--nightly", "latest"]

testFullNightly :: TestTree
testFullNightly = testCase "--nightly 2023-03-14" $
  runsFull args $ \resp ->
    expectedNightly @=? resp.snapshot
  where
    args = ["--nightly", "2023-03-14"]
    expectedNightly =
      MkSnapshotResp
        { ghc = "9.4.4",
          created = "2023-03-14",
          name = "nightly-2023-03-14",
          compiler = "ghc-9.4.4"
        }

testFullLtsLatest :: TestTree
testFullLtsLatest = testCase "--lts latest" $ runsFull args (const $ pure ())
  where
    args = ["--lts", "latest"]

testFullLts :: TestTree
testFullLts = testCase "--lts 20.14" $
  runsFull args $ \resp ->
    expectedLts @=? resp.snapshot
  where
    args = ["--lts", "20.14"]
    expectedLts =
      MkSnapshotResp
        { ghc = "9.2.7",
          created = "2023-03-12",
          name = "lts-20.14",
          compiler = "ghc-9.2.7"
        }

testFull :: TestTree
testFull =
  testCase "No arg uses implicit nightly" $ runsFull [] (const $ pure ())

testFullNightlyOverridesLts :: TestTree
testFullNightlyOverridesLts = testCase "--nightly overrides lts" $
  runsFull args $
    \resp -> expectedNightly @=? resp.snapshot
  where
    args = ["--nightly", "2023-03-14", "--lts", "latest"]
    expectedNightly =
      MkSnapshotResp
        { ghc = "9.4.4",
          created = "2023-03-14",
          name = "nightly-2023-03-14",
          compiler = "ghc-9.4.4"
        }

runsFull :: [String] -> (StackageResp -> IO a) -> IO a
runsFull args expect = do
  result <- T.unlines <$> run snapshotArgs

  case decodeStr @StackageResp result of
    Left err -> assertFailure $ "Could not decode StackageResp: " <> err
    Right resp -> expect resp
  where
    snapshotArgs = args <> ["full"]

testSnapshot :: TestTree
testSnapshot = testCase "Snapshot command" $ do
  result <- T.unlines <$> run args

  whenLeft (decodeStr @SnapshotResp result) $ \err ->
    assertFailure $ "Could not decode StackageResp: " <> err
  where
    args = ["snapshot"]

testExclude :: TestTree
testExclude = testCase "Excludes packages" $ do
  results <- run args
  -- verify that the pipes-* libs are excluded from the results, minus the
  -- sole one left off: pipes-parse
  ["pipes-parse"] @=? filter (`Set.member` exclude) (fmap dropVersion results)
  where
    args = ["--lts", "20.14", "--exclude", "./examples/exclude/pipes-no-parse", "pkgs"]
    exclude = Set.fromList pipesLibs

testInclude :: TestTree
testInclude = testCase "Includes packages" $ do
  results <- run args
  pipesLibs @=? fmap dropVersion results
  where
    args = ["--lts", "20.14", "--include", "./examples/include/pipes", "pkgs"]

testIncludeExclude :: TestTree
testIncludeExclude = testCase "Exclude + include packages" $ do
  results <- run args
  -- pipes-parse is the only pkg in include that is not in exclude
  ["pipes-parse"] @=? fmap dropVersion results
  where
    args =
      [ "--lts",
        "20.14",
        "-i",
        "./examples/include/pipes",
        "-e",
        "./examples/exclude/pipes-no-parse",
        "pkgs"
      ]

pipesLibs :: [Text]
pipesLibs =
  [ "pipes",
    "pipes-attoparsec",
    "pipes-bytestring",
    "pipes-concurrency",
    "pipes-csv",
    "pipes-extras",
    "pipes-fastx",
    "pipes-fluid",
    "pipes-group",
    "pipes-mongodb",
    "pipes-ordered-zip",
    "pipes-parse",
    "pipes-random",
    "pipes-safe",
    "pipes-text",
    "pipes-wai"
  ]

test404 :: TestTree
test404 = testCase "Throws 404" $ do
  try @StackageException badRun >>= \case
    Left ex@(MkStackageException _ _) ->
      case T.stripPrefix prefix (T.pack $ displayException ex) of
        Nothing ->
          assertFailure $
            "Exception did not match expected text: " <> displayException ex
        Just _ -> pure ()
    Right _ -> assertFailure "Expected 404 exception, received none."
  where
    prefix = "Received 404 for snapshot: lts-00.99. Is the snapshot correct?"
    badRun = withArgs args $ withStackageParser (const (pure ()))
    args = ["--lts", "00.99", "pkgs"]

run :: [String] -> IO [Text]
run args = do
  ref <- newIORef []
  withArgs args $ withStackageParser (\s -> modifyIORef' ref (s :))
  reverse <$> readIORef ref

decodeStr :: (FromJSON a) => Text -> Either String a
decodeStr = Asn.eitherDecodeStrict' . TEnc.encodeUtf8

whenLeft :: (Applicative f) => Either e a -> (e -> f ()) -> f ()
whenLeft (Left e) f = f e
whenLeft _ _ = pure ()

dropVersion :: Text -> Text
dropVersion = T.intercalate "-" . dropLast . T.split (== '-')

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x : xs) = x : dropLast xs
