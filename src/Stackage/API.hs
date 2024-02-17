-- | REST API for stackage.org.
--
-- @since 0.1
module Stackage.API
  ( withResponse,
  )
where

import Data.Text qualified as T
import Network.HTTP.Client (BodyReader, Request, Response)
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as TLS
import Stackage.Data.Request (SnapshotIdReq (MkSnapshotIdReq))

withResponse :: SnapshotIdReq -> (Response BodyReader -> IO a) -> IO a
withResponse idReq onResponse = do
  manager <- TLS.newTlsManager
  req <- getRequest idReq
  HttpClient.withResponse req manager onResponse

getRequest :: SnapshotIdReq -> IO Request
getRequest (MkSnapshotIdReq idReq) = updateReq <$> mkReq
  where
    url = "https://stackage.org/" <> T.unpack idReq
    mkReq = HttpClient.parseRequest url
    updateReq r =
      r
        { HttpClient.requestHeaders =
            [ ("Accept", "application/json;charset=utf-8,application/json")
            ]
        }
