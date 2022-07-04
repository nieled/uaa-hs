module Domain.Auth where

import           Control.Monad.Except
    ( ExceptT (ExceptT)
    , MonadError (throwError)
    , MonadTrans (lift)
    , runExceptT
    )
import           Data.Text            ( Text, unpack )
import           Domain.Validation    ( lengthBetween, regexMatches, validate )
import           Text.RawString.QQ    ( r )

newtype Email
  = Email { emailRaw :: Text }
  deriving (Eq, Show)
rawEmail :: Email -> Text
rawEmail = emailRaw
mkEmail :: Text -> Either [EmailValidationErr] Email
mkEmail = validate Email
  [ regexMatches
      [r|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
      EmailValidationErrInvalidEmail
  ]

newtype Password
  = Password { passwordRaw :: Text }
  deriving (Eq, Show)
rawPassword :: Password -> Text
rawPassword = passwordRaw
mkPassword :: Text -> Either [PasswordValidationErr] Password
mkPassword = validate Password
  [ regexMatches [r|[0-9]|] PasswordValidationErrMustContainNumber
  , regexMatches [r|[A-Z]|] PasswordValidationErrMustContainUpperCase
  , regexMatches [r|[a-z]|] PasswordValidationErrMustContainLowerCase
  , lengthBetween 5 50 PasswordValidationErrLength
  ]

type VerificationCode = Text
newtype UserId
  = UserId Int
type SessionId = Text
data LoginError
  = LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Eq, Show)

data Auth
  = Auth
      { authEmail    :: Email
      , authPassword :: Password
      }
  deriving (Eq, Show)

-- Errors
data RegistrationError
  = RegistrationErrorEmailTaken
  deriving (Eq, Show)
data EmailValidationErr
  = EmailValidationErrInvalidEmail
  deriving (Eq, Show)
data PasswordValidationErr
  = PasswordValidationErrLength
  | PasswordValidationErrMustContainUpperCase
  | PasswordValidationErrMustContainLowerCase
  | PasswordValidationErrMustContainNumber
  deriving (Show)
data EmailVerificationError
  = EmailVerificationErrorInvalidCode
  deriving (Eq, Show)

-- Registration
class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

register
  :: (AuthRepo m, EmailVerificationNotif m)
  => Auth
  -> m (Either RegistrationError ())
register auth = runExceptT $ do
  verificationCode <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email verificationCode

verifyEmail
  :: AuthRepo m
  => VerificationCode
  -> m (Either EmailVerificationError ())
verifyEmail = setEmailAsVerified

login :: (AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing           -> throwError LoginErrorInvalidAuth
    Just (_  , False) -> throwError LoginErrorEmailNotVerified
    Just (uId, _    ) -> lift $ newSession uId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId
