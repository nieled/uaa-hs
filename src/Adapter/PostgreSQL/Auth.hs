module Adapter.PostgreSQL.Auth where

import           Control.Exception.Safe                        ( throwString )
import           Data.Pool
import           Database.PostgreSQL.Simple
-- TODO upgrade to postgresql-migration v2
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
