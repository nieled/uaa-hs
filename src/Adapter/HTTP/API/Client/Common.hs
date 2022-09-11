module Adapter.HTTP.API.Client.Common where

import           Data.Aeson
import           Data.Has
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

type HttpClient r m = (MonadReader r m, Has State r, MonadIO m, MonadThrow m)

newtype Config = Config { configUrl :: String }
data State = State
  { stateInitReq :: Request
  , stateManager :: Manager
  }


