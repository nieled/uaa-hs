module Lib
  ( main
  ) where

import qualified Adapter.InMemory.Auth  as M
import           Control.Concurrent.STM ( TVar, newTVarIO )
import           Control.Monad.Reader
    ( MonadIO (..), MonadReader, ReaderT (..) )
import           Debug.Trace            ( trace, traceId )
import           Domain.Auth

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
