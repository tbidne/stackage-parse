-- | Types returned by stackage API.
--
-- @since 0.1
module Stackage.Data.Response
  ( StackageResp (..),
    SnapshotResp (..),
    PackageResp (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Response returned by primary stackage endpoint e.g.
-- @stackage.org\/lts-20.14@.
--
-- @since 0.1
data StackageResp = MkStackageResp
  { -- | @since 0.1
    snapshot :: !SnapshotResp,
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
  deriving anyclass
    ( -- | @since 0.1
      FromJSON,
      -- | @since 0.1
      ToJSON
    )

-- | Stackage snapshot data.
--
-- @since 0.1
data SnapshotResp = MkSnapshotResp
  { -- | @since 0.1
    ghc :: !Text,
    -- | @since 0.1
    created :: !Text,
    -- | @since 0.1
    name :: !Text,
    -- | @since 0.1
    compiler :: !Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      FromJSON,
      -- | @since 0.1
      ToJSON
    )

-- | Package in a stackage snapshot.
--
-- @since 0.1
data PackageResp = MkPackageResp
  { -- | @since 0.1
    origin :: !Text,
    -- | @since 0.1
    name :: !Text,
    -- | @since 0.1
    version :: !Text,
    -- | @since 0.1
    synopsis :: !Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      FromJSON,
      -- | @since 0.1
      ToJSON
    )
