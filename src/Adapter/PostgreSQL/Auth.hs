module Adapter.PostgreSQL.Auth where

import           Control.Exception.Safe                        ( throwString )
import           Data.ByteString
-- TODO upgrade to postgresql-migration v2
import           Data.Pool
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration.V1Compat

type State = Pool Connection

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

data Config
  = Config
      { configUrl                  :: ByteString
      , configStripeCount          :: Int
      , configMaxOpenConnPerStripe :: Int
      , configIdleConnTimeout      :: NominalDiffTime
      }

-- withPool :: Config -> (State -> IO a) -> IO a
-- withPool config action = bracket initPool cleanPool action
--  where
--   initPool = newPool openConn
--                      closeConn
--                      (configStripeCount config)
--                      (configIdleConnTimeout config)
--                      (configMaxOpenConnPerStripe config)
--   cleanPool = destroyAllResources
--   openConn  = connectPostgreSQL (configUrl config)
--   closeConn = close

