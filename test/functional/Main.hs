module Main (main) where

import Control.Exception (Exception (..), try)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Asn
import Data.Foldable (traverse_)
import Data.HashSet qualified as Set
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Version.Package qualified as PV
import Stackage (StackageException404 (..))
import Stackage.Data.Response (SnapshotResp (..), StackageResp (..))
import Stackage.Runner (withStackageParser)
import System.Environment (withArgs)
import System.Environment.Guard (ExpectEnv (..), guardOrElse')
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))

main :: IO ()
main = guardOrElse' "RUN_FUNCTIONAL" ExpectEnvSet runTests warn
  where
    runTests = Tasty.defaultMain tests
    warn = putStrLn "*** Functional tests disabled. Enable with RUN_FUNCTIONAL=1. ***"

tests :: TestTree
tests = do
  Tasty.testGroup
    "Functional Tests"
    [ pkgsTests,
      fullTests,
      testSnapshot,
      testExclude,
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
  -- verify the below packages are excluded from the results
  [] @=? filter (`Set.member` excluded) (fmap dropVersion results)
  where
    args = ["--lts", "20.14", "--exclude", "./examples/exclusions", "pkgs"]
    excluded =
      Set.fromList
        [ "pipes",
          "pipes-attoparsec",
          "pipes-bytestring",
          "pipes-concurrency",
          "pipes-csv",
          "pipes-extras",
          "pipes-fastx",
          "pipes-fluid",
          "pipes-group",
          "pipes-http",
          "pipes-mongodb",
          "pipes-parse",
          "pipes-random",
          "pipes-wai"
        ]
    dropVersion = T.intercalate "-" . dropLast . T.split (== '-')
    dropLast [] = []
    dropLast [_] = []
    dropLast (x : xs) = x : dropLast xs

test404 :: TestTree
test404 = testCase "Throws 404" $ do
  try @StackageException404 badRun >>= \case
    Left (MkStackageException404 _ _) -> pure ()
    Right _ -> assertFailure "Expected 404 exception, received none."
  where
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
