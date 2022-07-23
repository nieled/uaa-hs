module Lib
  ( main
  ) where

import qualified Adapter.HTTP.Main             as HTTP
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
import           Domain.Auth                    ( Auth(Auth)
                                                , AuthRepo(..)
                                                , EmailVerificationNotif(..)
                                                , SessionRepo(..)
                                                , getUser
                                                , login
                                                , mkEmail
                                                , mkPassword
                                                , register
                                                , resolveSessionId
                                                , verifyEmail
                                                )
import           Katip                          ( ColorStrategy(ColorIfTerminal)
                                                , Katip
                                                , KatipContext
                                                , KatipContextT
                                                , LogEnv
                                                , Severity(InfoS)
                                                , Verbosity(V2)
                                                , closeScribes
                                                , defaultScribeSettings
                                                , initLogEnv
                                                , mkHandleScribe
                                                , permitItem
                                                , registerScribe
                                                , runKatipContextT
                                                )
import           System.IO                      ( stdout )
import           Text.StringRandom              ( stringRandomIO )

main :: IO ()
main = withState $ \port le state@(_, _, mqState, _) -> do
  let runner = run le state
  MQAuth.init mqState runner
  HTTP.main port runner

withState :: (Int -> LogEnv -> State -> IO ()) -> IO ()
withState action = withKatip $ \le -> do
  memoryState <- newTVarIO M.initialState
  PG.withState pgCfg $ \pgState -> Redis.withState redisCfg $ \redisState ->
    MQ.withState mqCfg 16 $ \mqState -> do
      let state = (pgState, redisState, mqState, memoryState)
      action port le state
 where
  -- TODO: parse from ENV variables
  mqCfg = "amqp://guest:guest@localhost:5672/%2F"
  pgCfg = PG.Config { PG.configDatabase             = "uaa"
                    , PG.configUser                 = "uaa"
                    , PG.configPassword             = "uaa"
                    , PG.configStripeCount          = 2
                    , PG.configMaxOpenConnPerStripe = 5
                    , PG.configIdleConnTimeout      = 10
                    }
  port     = 3000
  redisCfg = "redis://localhost:6379/0"

withKatip :: (LogEnv -> IO a) -> IO a
withKatip = bracket createLogEnv closeScribes
 where
  createLogEnv = do
    let logEnv = initLogEnv "uaa-hs" "dev"
    stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
    registerScribe "stdout" stdoutScribe defaultScribeSettings =<< logEnv

-- No longer being called in main as `runner action`
-- Now scottyT blocks forever to handle any requests, so we no run test like this
action :: App ()
action = do
  rndEmail <- liftIO $ stringRandomIO "[a-z0-9]{5}@test\\.com"
  let email = either undefined id . mkEmail $ rndEmail
      passw = either undefined id $ mkPassword "iH8sn0w"
      auth  = Auth email passw
  register auth
  verificationCode <- pollNotif email
  verifyEmail verificationCode
  Right session         <- login auth
  Just  userId          <- resolveSessionId session
  Just  registeredEmail <- getUser userId
  liftIO $ print (session, userId, registeredEmail)
  return ()
 where
  pollNotif email = do
    result <- M.getNotificationsForEmail email
    case result of
      Nothing               -> pollNotif email
      Just verificationCode -> return verificationCode

type State = (PG.State, Redis.State, MQ.State, TVar M.State)

newtype App a = App {unApp :: ReaderT State (KatipContextT IO) a}
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
