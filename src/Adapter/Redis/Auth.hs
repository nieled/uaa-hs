module Adapter.Redis.Auth where

import           Control.Exception.Safe         ( throwString )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.Reader           ( MonadIO(..)
                                                , MonadReader
                                                , ReaderT(..)
                                                , asks
                                                )
import           Data.ByteString.Char8          ( pack )
import           Data.Has                       ( Has(getter) )
import           Data.Text                      ( unpack )
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import qualified Database.Redis                as R
import qualified Domain.Auth                   as D
import           Text.Read                      ( readMaybe )
import           Text.StringRandom              ( stringRandomIO )

type State = R.Connection

type Redis r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

-- | Create state from Redis URL String
withState :: String -> (State -> IO a) -> IO a
withState connURL action = do
  case R.parseConnectInfo connURL of
    Left  _        -> throwString "Invalid Redis connection URL"
    Right connInfo -> do
      conn <- R.checkedConnect connInfo
      action conn

-- | Helper function to execute `R.Redis` under `Redis r m` constraint
withConn :: Redis r m => R.Redis a -> m a
withConn action = do
  conn <- asks getter
  liftIO $ R.runRedis conn action

-- | Creates a new session from a random string and store the kv sessionId:userId
newSession :: Redis r m => D.UserId -> m D.SessionId
newSession userId = do
  sId    <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
  result <- withConn $ R.set (encodeUtf8 sId) (pack . show $ userId)
  case result of
    Right R.Ok -> return sId
    err        -> throwString $ "Unexpected Redis error: " <> show err

findUserIdBySessionId :: Redis r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sessionId = do
  result <- withConn $ R.get (encodeUtf8 sessionId)
  case result of
    Right (Just userIdStr) ->
      return . readMaybe . unpack . decodeUtf8 $ userIdStr
    err -> throwString $ "Unexpected Redis error: " <> show err
