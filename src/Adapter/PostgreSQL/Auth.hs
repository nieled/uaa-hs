module Adapter.PostgreSQL.Auth where

import           Control.Exception                             ( bracket, try )
import           Control.Exception.Safe                        ( throwString )
import           Control.Monad.Catch                           ( MonadThrow )
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

-- | Constraint synonym that states that m is a monad and:
-- Perform IO action (via MonadIO)
-- Throw an exception (via MonadThrow)
-- Read r from environment (via MonadReader r)
-- Get State from r (via Has State r)
type PG r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

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
addAuth (D.Auth email pass) = do
  let rawEmail    = D.rawEmail email
      rawPassword = D.rawPassword pass
  verificationCode <- liftIO $ do
    r <- stringRandomIO "[A-Za-z0-9]{16}"
    return $ rawEmail <> "_" <> r
  result <- withConn $ \conn ->
    try $ query conn sqlStr (rawEmail, rawPassword, verificationCode)
  case result of
    Right [Only userId] -> return $ Right $ (userId, verificationCode)
    Right _ -> throwString "Should not happen: PG doesn't return userId"
    Left err@SqlError { sqlState = state, sqlErrorMsg = msg } ->
      if state == "23505" && "auths_email_key" `isInfixOf` msg
        then return $ Left D.RegistrationErrorEmailTaken
        else throwString $ "Unhandled PG exception: " <> show err
 where
  sqlStr
    = "INSERT INTO auths \
    \(email, pass, email_verification_code, is_email_verified) \
    \VALUES (?, crypt(?, gen_salt('bf')), ?, 'f') \
    \RETURNING id"
