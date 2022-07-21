module Lib
  ( main
  ) where

import qualified Adapter.InMemory.Auth         as M
import qualified Adapter.PostgreSQL.Auth       as PG
import qualified Adapter.RabbitMQ.Auth         as MQAuth
import qualified Adapter.RabbitMQ.Common       as MQ
import qualified Adapter.Redis.Auth            as Redis
import           Control.Concurrent.STM         ( TVar
                                                , newTVarIO
                                                )
import           Control.Exception              ( bracket )
import           Control.Monad.Catch            ( MonadCatch
                                                , MonadThrow
                                                )
import           Control.Monad.Reader           ( MonadIO(..)
                                                , MonadReader
                                                , ReaderT(..)
                                                )
import           Debug.Trace                    ( trace
                                                , traceId
                                                )
import           Domain.Auth
import           Katip
import           System.IO                      ( stdout )
import           Text.StringRandom

main :: IO ()
main = withState $ \le state@(_, _, mqState, _) -> do
  let runner = run le state
  MQAuth.init mqState runner
  runner action

withState :: (LogEnv -> State -> IO ()) -> IO ()
withState action = withKatip $ \le -> do
  memoryState <- newTVarIO M.initialState
  PG.withState pgCfg $ \pgState -> Redis.withState redisCfg $ \redisState ->
    MQ.withState mqCfg 16 $ \mqState -> do
      let state = (pgState, redisState, mqState, memoryState)
      action le state
 where
  -- TODO: parse from ENV variables
  mqCfg = "amqp://guest:guest@localhost:5672/%2F"
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
  rndEmailSuffix <- liftIO $ stringRandomIO "[a-z0-9]{4}"
  let email =
        either undefined id
          .  mkEmail
          $  "nieled."
          <> rndEmailSuffix
          <> "@riseup.net"
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

type State = (PG.State, Redis.State, MQ.State, TVar M.State)

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
    , MonadCatch
    )

run :: LogEnv -> State -> App a -> IO a
run le state = runKatipContextT le () mempty . flip runReaderT state . unApp

instance AuthRepo App where
  addAuth             = PG.addAuth
  setEmailAsVerified  = PG.setEmailAsVerified
  findUserByAuth      = PG.findUserByAuth
  findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
  -- In memory notification store
  -- notifyEmailVerification = M.notifyEmailVerification
  notifyEmailVerification = MQAuth.notifyEmailVerification

instance SessionRepo App where
  -- In-memory session mngmt
  -- newSession            = M.newSession
  -- findUserIdBySessionId = M.findUserIdBySessionId
  newSession            = Redis.newSession
  findUserIdBySessionId = Redis.findUserIdBySessionId
