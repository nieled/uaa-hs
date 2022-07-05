module Lib
  ( main
  ) where

import qualified Adapter.InMemory.Auth  as M
import           Control.Concurrent.STM ( TVar, newTVarIO )
import           Control.Exception      ( bracket )
import           Control.Monad.Reader
    ( MonadIO (..), MonadReader, ReaderT (..) )
import           Debug.Trace            ( trace, traceId )
import           Domain.Auth
import           Katip
import           System.IO              ( stdout )

main :: IO ()
main = do
  state <- newTVarIO M.initialState
  run state action

action :: App ()
action = do
  let email = either undefined id $ mkEmail "nieled@riseup.net"
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

type State = TVar M.State
newtype App a
  = App { unApp :: ReaderT State IO a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO, MonadReader State)

run :: State -> App a -> IO a
run state = flip runReaderT state . unApp

instance AuthRepo App where
  addAuth             = M.addAuth
  setEmailAsVerified  = M.setEmailAsVerified
  findUserByAuth      = M.findUserByAuth
  findEmailFromUserId = M.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  newSession            = M.newSession
  findUserIdBySessionId = M.findUserIdBySessionId

----------------------------------------------------------------
-- TODO: implement Katip
runKatip :: IO ()
runKatip = withKatip $ \le -> do
  let initialContext   = () -- this context will be attached to every log in your app and merged w/ subsequent contexts
      initialNamespace = "main"
  runKatipContextT le initialContext initialNamespace logSomething

withKatip :: (LogEnv -> IO a) -> IO a
withKatip = bracket createLogEnv closeScribes
 where
  createLogEnv = do
    stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
    registerScribe "stdout" stdoutScribe defaultScribeSettings
      =<< initLogEnv "uaa-hs" "dev"

logSomething :: (KatipContext m) => m ()
logSomething = do
  $(logTM) InfoS "Hello Katip"
  -- This adds a namespace to the current namespace and merges a piece of contextual data into your context
  katipAddNamespace "additional_namespace" $ katipAddContext (sl "some_context" True) $ do
    $(logTM) WarningS "Now we're getting fancy"
  katipNoLogging $ do
    $(logTM) DebugS "You will never see this!"
----------------------------------------------------------------
