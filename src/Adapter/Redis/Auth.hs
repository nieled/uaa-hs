module Adapter.Redis.Auth where

import           Control.Exception.Safe         ( throwString )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.Reader           ( MonadIO(..)
                                                , MonadReader
                                                , ReaderT(..)
                                                )
import           Data.Has
import qualified Database.Redis                as R
import qualified Domain.Auth                   as D
import           Text.StringRandom

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

withConn :: Redis r m => R.Redis a -> m a
withConn action = do
  undefined

newSession :: Redis r m => D.UserId -> m D.SessionId
newSession userId = undefined

findUserIdBySessionId :: Redis r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId = undefined