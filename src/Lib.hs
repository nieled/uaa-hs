module Lib
  ( main
  ) where

import qualified Adapter.InMemory.Auth   as M
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Adapter.Redis.Auth      as Redis
import           Control.Concurrent.STM  ( TVar, newTVarIO )
import           Control.Exception       ( bracket )
import           Control.Monad.Catch     ( MonadThrow )
import           Control.Monad.Reader
    ( MonadIO (..), MonadReader, ReaderT (..) )
import           Debug.Trace             ( trace, traceId )
import           Domain.Auth
import           Katip
import           System.IO               ( stdout )


main :: IO ()
main = withKatip $ \le -> do
  memoryState <- newTVarIO M.initialState
  PG.withState pgCfg $ \pgState -> Redis.withState redisCfg
    $ \redisState -> run le (pgState, redisState, memoryState) action
 where
  -- TODO: parse from ENV variables
  pgCfg = PG.Config { PG.configUrl = "postgresql://localhost/uaa"
                    , PG.configStripeCount          = 2
                    , PG.configMaxOpenConnPerStripe = 5
                    , PG.configIdleConnTimeout      = 10
                    }
  redisCfg = "redis://localhost:6379/0"

withKatip :: (LogEnv -> IO a) -> IO a
withKatip = bracket createLogEnv closeScribes
 where
  createLogEnv = do
    let logEnv = initLogEnv "uaa-hs" "dev"
    stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
    registerScribe "stdout" stdoutScribe defaultScribeSettings =<< logEnv

action :: App ()
action = do
  let email = either undefined id $ mkEmail "nieled.003@riseup.net"
      passw = either undefined id $ mkPassword "iH8sn0w"
      auth  = Auth email passw
  register auth
  Just vCode <- M.getNotificationsForEmail email
  verifyEmail vCode
  Right session         <- login auth
  Just  uId             <- resolveSessionId session
  Just  registeredEmail <- getUser uId
  liftIO $ print (session, uId, registeredEmail)
  return ()

type State = (PG.State, Redis.State, TVar M.State)

newtype App a
  = App { unApp :: ReaderT State (KatipContextT IO) a }
  -- = App { unApp :: KatipContextT (ReaderT State IO) a } -- same as above
  deriving
    ( Applicative
    , Functor
    , Katip
    , KatipContext
    , Monad
    , MonadFail
    , MonadIO
    , MonadReader State
    , MonadThrow
    )

run :: LogEnv -> State -> App a -> IO a
run le state = runKatipContextT le () mempty . flip runReaderT state . unApp

instance AuthRepo App where
  addAuth             = PG.addAuth
  setEmailAsVerified  = PG.setEmailAsVerified
  findUserByAuth      = PG.findUserByAuth
  findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  -- In-memory session mngmt
  -- newSession            = M.newSession
  -- findUserIdBySessionId = M.findUserIdBySessionId
  newSession            = Redis.newSession
  findUserIdBySessionId = Redis.findUserIdBySessionId
