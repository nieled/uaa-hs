module Adapter.InMemory.Auth where

import           Control.Concurrent.STM
import           Control.Monad.Except
    ( MonadError, MonadTrans (lift), runExceptT, throwError, when )
import           Control.Monad.IO.Class ( MonadIO (liftIO) )
import           Control.Monad.Reader   ( MonadReader, asks )
import           Data.Containers        ( deleteMap, insertMap, insertSet )
import           Data.Foldable          ( find )
import           Data.Has               ( Has (getter) )
import           Data.Map               as Map
import           Data.Set               as Set
import           Data.Text              ( pack )
import qualified Domain.Auth            as D
import           Text.StringRandom      ( stringRandomIO )


data State
  = State
      { stateAuths            :: [(D.UserId, D.Auth)]
      , stateUnverifiedEmails :: Map D.VerificationCode D.Email
      , stateVerifiedEmails   :: Set D.Email
      , stateUserIdCounter    :: Int
      , stateNotifications    :: Map D.Email D.VerificationCode
      , stateSessions         :: Map D.SessionId D.UserId
      }
  deriving (Eq, Show)

initialState :: State
initialState = State { stateAuths            = []
                     , stateUnverifiedEmails = mempty
                     , stateVerifiedEmails   = mempty
                     , stateUserIdCounter    = 0
                     , stateNotifications    = mempty
                     , stateSessions         = mempty
                     }

-- | The following computation works for any m that is an instance of MonadIO and MonadReader r,
-- where r is any structure that has TVar State.
-- This definition needs ConstraintKinds, otherwise we would repeat this on each function signature.
type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

addAuth
  :: InMemory r m
  => D.Auth
  -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth auth = do
  tvar  <- asks getter
  -- gen verification code
  vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
  liftIO . atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    -- check whether the given email is duplicate
    let auths       = stateAuths state
        email       = D.authEmail auth
        isDuplicate = any ((email ==) . D.authEmail . snd) auths
    when isDuplicate $ throwError D.RegistrationErrorEmailTaken
    -- update the state
    let newUserId      = stateUserIdCounter state + 1
        newAuths       = (newUserId, auth) : auths
        unverifieds    = stateUnverifiedEmails state
        newUnverifieds = insertMap vCode email unverifieds
        newState       = state { stateAuths            = newAuths
                               , stateUserIdCounter    = newUserId
                               , stateUnverifiedEmails = newUnverifieds
                               }
    lift $ writeTVar tvar newState
    return (newUserId, vCode)

orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow Nothing e  = throwError e
orThrow (Just a) _ = return a

setEmailAsVerified
  :: InMemory r m
  => D.VerificationCode
  -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
  tvar <- asks getter
  liftIO . atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let unverifieds = stateUnverifiedEmails state
        mEmail      = Map.lookup vCode unverifieds
    email <- mEmail `orThrow` D.EmailVerificationErrorInvalidCode
    let auths   = stateAuths state
        mUserId = fst <$> find ((email ==) . D.authEmail . snd) auths
    uId <- mUserId `orThrow` D.EmailVerificationErrorInvalidCode
    let verifieds      = stateVerifiedEmails state
        newVerifieds   = insertSet email verifieds
        newUnverifieds = deleteMap vCode unverifieds
        newState       = state { stateUnverifiedEmails = newUnverifieds
                               , stateVerifiedEmails   = newVerifieds
                               }
    lift $ writeTVar tvar newState
    return (uId, email)

findUserByAuth
  :: InMemory r m
  => D.Auth
  -> m (Maybe (D.UserId, Bool))
findUserByAuth auth = do
  tvar  <- asks getter
  state <- liftIO $ readTVarIO tvar
  let mUserId = fmap fst . find ((auth ==) . snd) $ stateAuths state
  case mUserId of
    Nothing  -> return Nothing
    Just uId -> do
      let verifieds  = stateVerifiedEmails state
          email      = D.authEmail auth
          isVerified = email `elem` verifieds
      return $ Just (uId, isVerified)

findEmailFromUserId
  :: InMemory r m
  => D.UserId
  -> m (Maybe D.Email)
findEmailFromUserId uId = do
  tvar  <- asks getter
  state <- liftIO $ readTVarIO tvar
  let mAuth = fmap snd . find ((uId ==) . fst) $ stateAuths state
  return $ D.authEmail <$> mAuth

-- | Only for testing purposes.
-- This implementation fakes sending notifications by storing it in memory,
-- there is no way to get the verification code unless we provide a function to get it.
getNotificationsForEmail
  :: InMemory r m
  => D.Email
  -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
  tvar  <- asks getter
  state <- liftIO $ readTVarIO tvar
  return $ Map.lookup email $ stateNotifications state

notifyEmailVerification
  :: InMemory r m
  => D.Email
  -> D.VerificationCode
  -> m ()
notifyEmailVerification email vCode = do
  tvar <- asks getter
  liftIO . atomically $ do
    state <- readTVar tvar
    let notifications    = stateNotifications state
        newNotifications = insertMap email vCode notifications
        newState         = state { stateNotifications = newNotifications }
    writeTVar tvar newState

newSession
  :: InMemory r m
  => D.UserId
  -> m D.SessionId
newSession uId = do
  tvar <- asks getter
  sId  <- liftIO $ ((pack . show $ uId) <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
  liftIO . atomically $ do
    state <- readTVar tvar
    let sessions    = stateSessions state
        newSessions = insertMap sId uId sessions
        newState    = state { stateSessions = newSessions }
    writeTVar tvar newState
    return sId

findUserIdBySessionId
  :: InMemory r m
  => D.SessionId
  -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  tvar <- asks getter
  liftIO $ Map.lookup sId . stateSessions <$> readTVarIO tvar
