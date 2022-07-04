module Domain.Auth where
import           Data.Text         ( Text )
import           Domain.Validation ( lengthBetween, regexMatches, validate )
import           Text.RawString.QQ ( r )

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
  = PasswordValidationErrLength
  | PasswordValidationErrMustContainUpperCase
  | PasswordValidationErrMustContainLowerCase
  | PasswordValidationErrMustContainNumber
  deriving (Show)
