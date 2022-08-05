module Adapter.HTTP.Web.Auth where

import           Adapter.HTTP.Web.Common
import           Control.Monad.Reader           ( MonadIO
                                                , lift
                                                )
import           Data.Text                      ( Text )
import           Domain.Auth
import           Katip
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A
-- import qualified Text.Digestive.Blaze.Html5    as DH
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

  get "/auth/register" $ undefined
  post "/auth/register" $ undefined

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
    userId <- reqCurrentUserId
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
