-- | Types returned by stackage API.
--
-- @since 0.1
module Stackage.Data.Response
  ( StackageResp (..),
    SnapshotResp (..),
    PackageResp (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Text.JSON
  ( JSON (readJSON, showJSON),
    JSObject,
    JSValue (JSObject),
    Result (Error),
  )
import Text.JSON qualified as JSON

-- | Response returned by primary stackage endpoint e.g.
-- @stackage.org\/lts-20.14@.
--
-- @since 0.1
data StackageResp = MkStackageResp
  { -- | @since 0.1
    snapshot :: SnapshotResp,
    -- | @since 0.1
    packages :: ![PackageResp]
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance JSON StackageResp where
  showJSON resp =
    JSON.makeObj
      [ ("snapshot", showJSON resp.snapshot),
        ("packages", showJSON resp.packages)
      ]

  readJSON = readJSONObj $ \v ->
    MkStackageResp
      <$> JSON.valFromObj "snapshot" v
      <*> JSON.valFromObj "packages" v

-- | Stackage snapshot data.
--
-- @since 0.1
data SnapshotResp = MkSnapshotResp
  { -- | @since 0.1
    ghc :: Text,
    -- | @since 0.1
    created :: Text,
    -- | @since 0.1
    name :: Text,
    -- | @since 0.1
    compiler :: Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance JSON SnapshotResp where
  showJSON resp =
    JSON.makeObj
      [ ("ghc", showJSON resp.ghc),
        ("created", showJSON resp.created),
        ("name", showJSON resp.name),
        ("compiler", showJSON resp.compiler)
      ]

  readJSON = readJSONObj $ \v ->
    MkSnapshotResp
      <$> JSON.valFromObj "ghc" v
      <*> JSON.valFromObj "created" v
      <*> JSON.valFromObj "name" v
      <*> JSON.valFromObj "compiler" v

-- | Package in a stackage snapshot.
--
-- @since 0.1
data PackageResp = MkPackageResp
  { -- | @since 0.1
    origin :: Text,
    -- | @since 0.1
    name :: Text,
    -- | @since 0.1
    version :: Text,
    -- | @since 0.1
    synopsis :: Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance JSON PackageResp where
  showJSON resp =
    JSON.makeObj
      [ ("origin", showJSON resp.origin),
        ("name", showJSON resp.name),
        ("version", showJSON resp.version),
        ("synopsis", showJSON resp.synopsis)
      ]

  readJSON = readJSONObj $ \v ->
    MkPackageResp
      <$> JSON.valFromObj "origin" v
      <*> JSON.valFromObj "name" v
      <*> JSON.valFromObj "version" v
      <*> JSON.valFromObj "synopsis" v

readJSONObj :: (JSObject JSValue -> Result a) -> JSValue -> Result a
readJSONObj onObj (JSObject obj) = onObj obj
readJSONObj _ other = Error $ "Expected json obj, received: " ++ show other
