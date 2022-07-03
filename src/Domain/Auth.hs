{-# LANGUAGE NamedFieldPuns #-}
module Domain.Auth where
import           Data.Text         ( Text )
import           Domain.Validation ( regexMatches, validate )
import           Text.RawString.QQ ( r )


newtype Email
  = Email { emailRaw :: Text }
  deriving (Eq, Show)

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [EmailValidationErr] Email
mkEmail = undefined

newtype Password
  = Password { passwordRaw :: Text }
  deriving (Eq, Show)

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [PasswordValidationErr] Password
mkPassword = undefined

data Auth
  = Auth
      { authEmail    :: Email
      , authPassword :: Password
      }
  deriving (Eq, Show)

data RegistrationError
  = RegistrationErrorEmailTaken
  deriving (Eq, Show)
data EmailValidationErr
  = EmailValidationErrInvalidEmail
  deriving (Eq, Show)
data PasswordValidationErr
  = PasswordValidationErrLength Int
  | PasswordValidationErrMustContainUpperCase
  | PasswordValidationErrMustContainLowerCase
  | PasswordValidationErrMustContainNumber
  deriving (Show)
