module Adapter.HTTP.API.Auth where

import           Adapter.HTTP.Common            ( parseAndValidateJSON
                                                , toResult
                                                )
import           Control.Monad.Reader           ( MonadIO )
import           Data.Text                      ( Text )
import           Domain.Auth                    ( Auth(Auth)
                                                , AuthRepo
                                                , EmailValidationErr
                                                , EmailVerificationError
                                                , EmailVerificationNotif
                                                , SessionRepo
                                                , mkEmail
                                                , mkPassword
                                                )
import           Katip                          ( KatipContext )
import qualified Text.Digestive                as DF
import           Text.Digestive.Form            ( (.:) )
import           Web.Scotty.Internal.Types      ( ScottyError
                                                , ScottyT
                                                )
import           Web.Scotty.Trans               ( get
                                                , post
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
  post "/api/auth/register" $ do
    input <- parseAndValidateJSON authForm
    undefined
  post "/api/auth/verifyEmail" undefined
  post "/api/auth/login"       undefined
  get "/api/users" undefined

authForm :: (Monad m) => DF.Form [EmailValidationErr] m Auth -- Domain.Auth.PasswordValidationErr
authForm = Auth <$> "email" .: emailForm <*> "password" .: passwordForm
 where
  emailForm    = DF.validate (toResult . mkEmail) (DF.text Nothing)
  passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)
