-- | REST API for stackage.org.
--
-- @since 0.1
module Stackage.API
  ( StackageAPI,
    stackageAPI,
    getStackageResp,
    getStackageClientEnv,
  )
where

import Data.Functor ((<&>))
import Data.Proxy (Proxy (..))
import Network.HTTP.Client.TLS qualified as TLS
import Servant.API (Capture, Get, JSON, (:>))
import Servant.Client (BaseUrl (..), ClientEnv, ClientM, Scheme (..))
import Servant.Client qualified as ServClient
import Stackage.Data.Request (SnapshotIdReq)
import Stackage.Data.Response (StackageResp)

-- | Stackage API
--
-- @since 0.1
type StackageAPI = Capture "snapshot" SnapshotIdReq :> Get '[JSON] StackageResp

-- | Stackage API
--
-- @since 0.1
stackageAPI :: Proxy StackageAPI
stackageAPI = Proxy

-- | GET 'StackageResp'.
--
-- @since 0.1
getStackageResp :: SnapshotIdReq -> ClientM StackageResp
getStackageResp = ServClient.client stackageAPI

-- | 'ClientEnv' for 'StackageAPI'.
--
-- @since 0.1
getStackageClientEnv :: IO ClientEnv
getStackageClientEnv =
  TLS.newTlsManager <&> \m -> ServClient.mkClientEnv m baseUrl
  where
    baseUrl =
      BaseUrl
        { baseUrlScheme = Https,
          baseUrlHost = "stackage.org",
          baseUrlPort = 443,
          baseUrlPath = "/"
        }
