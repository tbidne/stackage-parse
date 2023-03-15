module Main (main) where

import Control.Exception (Exception (..), try)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Asn
import Data.ByteString.Char8 qualified as Char8
import Data.Foldable (traverse_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as T
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
  results <- T.lines . T.pack <$> run pkgsArgs

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
  results <- T.lines . T.pack <$> run pkgsArgs

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
  testCase "--nightly latest" $
    runsFull args (const $ pure ())
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
testFullLtsLatest =
  testCase "--lts latest" $
    runsFull args (const $ pure ())
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
  testCase "No arg uses implicit nightly" $
    runsFull [] (const $ pure ())

testFullNightlyOverridesLts :: TestTree
testFullNightlyOverridesLts = testCase "--nightly overrides lts" $ do
  runsFull args $ \resp -> do
    expectedNightly @=? resp.snapshot
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
  result <- run snapshotArgs

  case decodeStr @StackageResp result of
    Left err -> assertFailure $ "Could not decode StackageResp: " <> err
    Right resp -> expect resp
  where
    snapshotArgs = args <> ["full"]

testSnapshot :: TestTree
testSnapshot = testCase "Snapshot command" $ do
  result <- run args

  whenLeft (decodeStr @SnapshotResp result) $ \err ->
    assertFailure $ "Could not decode StackageResp: " <> err
  where
    args = ["snapshot"]

test404 :: TestTree
test404 = testCase "Throws 404" $ do
  try @StackageException404 badRun >>= \case
    Left (MkStackageException404 _ _) -> pure ()
    Right _ -> assertFailure "Expected 404 exception, received none."
  where
    badRun = withArgs args $ withStackageParser (const (pure ()))
    args = ["--lts", "bad-snapshot", "pkgs"]

run :: [String] -> IO String
run args = do
  ref <- newIORef ""
  withArgs args $ withStackageParser (writeIORef ref)
  readIORef ref

decodeStr :: (FromJSON a) => String -> Either String a
decodeStr = Asn.eitherDecodeStrict' . Char8.pack

whenLeft :: (Applicative f) => Either e a -> (e -> f ()) -> f ()
whenLeft (Left e) f = f e
whenLeft _ _ = pure ()
