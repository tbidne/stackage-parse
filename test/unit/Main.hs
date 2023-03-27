module Main (main) where

import Stackage.Data.Request qualified as Request
import Stackage.Data.Request.Internal (SnapshotReq (..))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (testCase, (@=?))

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = do
  Tasty.testGroup
    "Unit Tests"
    [ requestTests
    ]

requestTests :: TestTree
requestTests =
  Tasty.testGroup
    "Stackage.Data.Request"
    [ testMkSnapshotReqLtsSuccess,
      testMkSnapshotReqLtsFailure,
      testMkSnapshotReqNightlySuccess,
      testMkSnapshotReqNightlyFailure
    ]

testMkSnapshotReqLtsSuccess :: TestTree
testMkSnapshotReqLtsSuccess =
  testCase "mkSnapshotReqLts succeeds" $
    Right (UnsafeSnapshotReqLts (Just "12.34"))
      @=? Request.mkSnapshotReqLts "12.34"

testMkSnapshotReqLtsFailure :: TestTree
testMkSnapshotReqLtsFailure = testCase "mkSnapshotReqLts fails" $ do
  Left "LTS snapshots have the form XX.YY, received '12'"
    @=? Request.mkSnapshotReqLts "12"
  Left "LTS snapshots have the form XX.YY, received '12.'"
    @=? Request.mkSnapshotReqLts "12."
  Left "LTS snapshots have the form XX.YY, received '.12'"
    @=? Request.mkSnapshotReqLts ".12"
  Left "LTS snapshots have the form XX.YY, received '  .12'"
    @=? Request.mkSnapshotReqLts "  .12"
  Left "LTS version should be an integer, received 'AB'"
    @=? Request.mkSnapshotReqLts "AB.CD"

testMkSnapshotReqNightlySuccess :: TestTree
testMkSnapshotReqNightlySuccess =
  testCase "mkSnapshotReqNightly succeeds" $
    Right (UnsafeSnapshotReqNightly (Just "2023-03-14"))
      @=? Request.mkSnapshotReqNightly "2023-03-14"

testMkSnapshotReqNightlyFailure :: TestTree
testMkSnapshotReqNightlyFailure = testCase "mkSnapshotReqNightly fails" $ do
  Left "Nightly snapshots have the form YYYY-MM-DD, received '2023-03'"
    @=? Request.mkSnapshotReqNightly "2023-03"
  Left "Nightly snapshots have the form YYYY-MM-DD, received '03-23'"
    @=? Request.mkSnapshotReqNightly "03-23"
  Left "Nightly snapshots have the form YYYY-MM-DD, received '  -03-23'"
    @=? Request.mkSnapshotReqNightly "  -03-23"
  Left "Nightly snapshots have the form YYYY-MM-DD, received '20230323'"
    @=? Request.mkSnapshotReqNightly "20230323"
  Left "Year should be an integer between 2000 and 2100, received 'YYYY'"
    @=? Request.mkSnapshotReqNightly "YYYY-MM-DD"
  Left "Day should be an integer between 1 and 31, received '32'"
    @=? Request.mkSnapshotReqNightly "2023-03-32"
  Left "Month should be an integer between 1 and 12, received '13'"
    @=? Request.mkSnapshotReqNightly "2023-13-30"
  Left "Year should be an integer between 2000 and 2100, received '1999'"
    @=? Request.mkSnapshotReqNightly "1999-12-30"
