module Adapter.HTTP.Web.Auth where

import           Adapter.HTTP.Common            ( setSessionIdInCookie
                                                , toResult
                                                )
import           Adapter.HTTP.Web.Common        ( formLayout
                                                , mainLayout
                                                , renderHtml
                                                , reqCurrentUserId
                                                )
import           Control.Arrow                  ( left )
import           Control.Monad.Reader           ( MonadIO
                                                , lift
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Domain.Auth                    ( Auth(Auth)
                                                , AuthRepo
                                                , EmailVerificationError
                                                  ( EmailVerificationErrorInvalidCode
                                                  )
                                                , EmailVerificationNotif
                                                , LoginError(..)
                                                , RegistrationError
                                                  ( RegistrationErrorEmailTaken
                                                  )
                                                , SessionRepo
                                                , getUser
                                                , login
                                                , mkEmail
                                                , mkPassword
                                                , rawEmail
                                                , register
                                                , verifyEmail
                                                )
import           Katip                          ( KatipContext )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Text.Digestive                as DF
import           Text.Digestive.Blaze.Html5    as DH
                                                ( inputPassword
                                                , inputText
                                                )
import           Text.Digestive.Form            ( (.:) )
import           Text.Digestive.Scotty          ( runForm )
import           Web.Scotty.Trans               ( ScottyError(stringError)
                                                , ScottyT
                                                , get
                                                , param
                                                , post
                                                , raise
                                                , redirect
                                                , rescue
                                                )


routes
  :: ( ScottyError e
     , MonadIO m
     , KatipContext m
     , AuthRepo m
     , EmailVerificationNotif m
     , SessionRepo m
     )
  => ScottyT e m ()
routes = do
  get "/" $ redirect "/users"

  get "/auth/register" $ do
    view <- DF.getForm "auth" authForm
    renderHtml $ registerPage view []
  post "/auth/register" $ do
    (view, mAuth) <- runForm "auth" authForm
    case mAuth of
      Nothing   -> renderHtml $ registerPage view []
      Just auth -> do
        result <- lift $ register auth
        case result of
          Left RegistrationErrorEmailTaken ->
            renderHtml $ registerPage view ["Email has been taken"]
          Right _ -> do
            v <- DF.getForm "auth" authForm
            renderHtml $ registerPage v ["Registered successfully"]

  get "/auth/verifyEmail/:code" $ do
    code   <- param "code" `rescue` const (return "")
    result <- lift $ verifyEmail code
    case result of
      Left EmailVerificationErrorInvalidCode ->
        renderHtml $ verifyEmailPage "The verification code is invalid"
      Right _ -> renderHtml $ verifyEmailPage "Your email has been verified"

  get "/auth/login" $ do
    view <- DF.getForm "auth" authForm
    renderHtml $ loginPage view []
  post "/auth/login" $ do
    (view, mAuth) <- runForm "auth" authForm
    case mAuth of
      Nothing   -> renderHtml $ loginPage view []
      Just auth -> do
        result <- lift $ login auth
        case result of
          Left LoginErrorEmailNotVerified ->
            renderHtml $ loginPage view ["Email has not been verified"]
          Left LoginErrorInvalidAuth ->
            renderHtml $ loginPage view ["Email or password is incorrect"]
          Right sessionId -> do
            setSessionIdInCookie sessionId
            redirect "/"

  get "/users" $ do
    userId <- Adapter.HTTP.Web.Common.reqCurrentUserId -- TODO
    mEmail <- lift $ getUser userId
    case mEmail of
      Nothing    -> raise $ stringError "Should not happen: email is not found"
      Just email -> renderHtml $ usersPage (rawEmail email)

usersPage :: Text -> H.Html
usersPage email = mainLayout "Users" $ do
  H.div $ H.h1 "Users"
  H.div $ H.toHtml email

verifyEmailPage :: Text -> H.Html
verifyEmailPage message = mainLayout "Email verification" $ do
  H.h1 "Email verification"
  H.div $ H.toHtml message
  H.div $ H.a ! A.href "/auth/login" $ "Login"

authForm :: (Monad m) => DF.Form [Text] m Auth
authForm = Auth <$> "email" .: emailForm <*> "password" .: passwordForm
 where
  emailForm    = DF.validate (toResult . asText . mkEmail) (DF.text Nothing)
  passwordForm = DF.validate (toResult . asText . mkPassword) (DF.text Nothing)
  asText :: (Show e) => Either [e] d -> Either [Text] d
  asText = left (pack . show <$>)

-- | Captures user registration info
authFormLayout :: DF.View [Text] -> Text -> Text -> [Text] -> H.Html
authFormLayout view formTitle action msgs = formLayout view action $ do
  H.h2 $ H.toHtml formTitle
  H.div $ errorList msgs
  H.div $ do
    H.label "Email"
    DH.inputText "email" view
    H.div $ errorList' "email"
  H.div $ do
    H.label "Password"
    DH.inputPassword "password" view
    H.div $ errorList' "password"
  H.input ! A.type_ "submit" ! A.value "Submit"
 where
  errorList' path = errorList . mconcat $ DF.errors path view
  errorList msgs = H.ul $ do
    H.li . H.toHtml . show $ msgs -- TODO: create a `H.li` for each error
  errorItem :: Text -> H.Html
  errorItem = H.li . H.toHtml

registerPage :: DF.View [Text] -> [Text] -> H.Html
registerPage view msgs = mainLayout "Register" $ do
  H.div $ authFormLayout view "Register" "/auth/register" msgs
  H.div $ H.a ! A.href "/auth/login" $ "Login"

loginPage :: DF.View [Text] -> [Text] -> H.Html
loginPage view msgs = mainLayout "Login" $ do
  H.div $ authFormLayout view "Login" "/auth/login" msgs
  H.div $ H.a ! A.href "/auth/register" $ "Register"
