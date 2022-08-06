module Adapter.HTTP.Web.Auth where

import           Adapter.HTTP.Common
import           Adapter.HTTP.Web.Common
import           Control.Arrow                  ( left )
import           Control.Monad.Reader           ( MonadIO
                                                , lift
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Domain.Auth
import           Katip
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Text.Digestive                as DF
import           Text.Digestive.Form            ( (.:) )
import           Text.Digestive.Scotty
import           Web.Scotty.Trans


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

  get "/auth/login" $ undefined
  post "/auth/login" $ undefined

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

registerPage :: DF.View [Text] -> [Text] -> H.Html
registerPage = undefined

authForm :: (Monad m) => DF.Form [Text] m Auth
authForm = Auth <$> "email" .: emailForm <*> "password" .: passwordForm
 where
  emailForm    = DF.validate (toResult . asText . mkEmail) (DF.text Nothing)
  passwordForm = DF.validate (toResult . asText . mkPassword) (DF.text Nothing)
  asText :: (Show e) => Either [e] d -> Either [Text] d
  asText = left (pack . show <$>)