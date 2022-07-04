module Adapter.InMemory.Auth where

import           Control.Concurrent.STM ( TVar )
import           Control.Monad.Except   ( MonadIO )
import           Control.Monad.Reader   ( MonadReader )
import           Data.Has               ( Has )
import           Data.Map               ( Map )
import           Data.Set               ( Set )
import           Domain.Auth            ( SessionRepo (findUserIdBySessionId) )
import qualified Domain.Auth            as D


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
  -> m (Either D.RegistrationError D.VerificationCode)
addAuth auth = undefined

setEmailAsVerified
  :: InMemory r m
  => D.VerificationCode
  -> m (Either D.EmailVerificationError ())
setEmailAsVerified = undefined

findUserByAuth
  :: InMemory r m
  => D.Auth
  -> m (maybe (D.UserId, Bool))
findUserByAuth = undefined

findEmailFromUserId
  :: InMemory r m
  => D.UserId
  -> m (Maybe D.Email)
findEmailFromUserId = undefined

notifyEmailVerification
  :: InMemory r m
  => D.Email
  -> D.VerificationCode
  -> m ()
notifyEmailVerification = undefined

newSession
  :: InMemory r m
  => D.UserId
  -> m D.SessionId
newSession = undefined

findUserIdBySessionId
  :: InMemory r m
  => D.SessionId
  -> m (Maybe D.UserId)
findUserIdBySessionId = undefined
