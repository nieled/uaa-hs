module Adapter.PostgreSQL.Auth where

import           Control.Exception                             ( bracket )
import           Control.Exception.Safe                        ( throwString )
import           Control.Monad.Reader
import           Data.ByteString
import           Data.Has
import           Data.Pool
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration.V1Compat
import qualified Domain.Auth                                   as D
import           Text.StringRandom

type State = Pool Connection

type PG r m = (Has State r, MonadReader r m, MonadIO m)

data Config
  = Config
      { configUrl                  :: ByteString
      , configStripeCount          :: Int
      , configMaxOpenConnPerStripe :: Int
      , configIdleConnTimeout      :: NominalDiffTime
      }

-- | Calls `withPool` and immediately executes migrations before
-- executing the `action`
withState :: Config -> (State -> IO a) -> IO a
withState config action = withPool config $ \state -> do
  migrate state
  action state

withPool :: Config -> (State -> IO a) -> IO a
withPool config action = bracket initPool cleanPool action
 where
  initPool = createPool openConn
                        closeConn
                        (configStripeCount config)
                        (configIdleConnTimeout config)
                        (configMaxOpenConnPerStripe config)
  cleanPool = destroyAllResources
  openConn  = connectPostgreSQL (configUrl config)
  closeConn = close

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO . withResource pool $ \conn -> action conn

migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _                  -> return ()
 where
  cmds =
    [ MigrationInitialization
    , MigrationDirectory "src/Adapter/PostgreSQL/Migrations"
    ]

addAuth
  :: PG r m
  => D.Auth
  -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth = undefined
