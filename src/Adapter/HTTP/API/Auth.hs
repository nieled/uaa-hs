{-# LANGUAGE OverloadedStrings #-}
module Adapter.HTTP.API.Auth where

import           Adapter.HTTP.Common            ( parseAndValidateJSON
                                                , reqCurrentUserId
                                                , setSessionIdInCookie
                                                , toResult
                                                )
import           Control.Arrow                  ( left )
import           Control.Monad.Reader           ( MonadIO
                                                , lift
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Domain.Auth                    ( Auth(..)
                                                , AuthRepo
                                                , EmailValidationErr
                                                , EmailVerificationError(..)
                                                , EmailVerificationNotif
                                                , LoginError(..)
                                                , RegistrationError(..)
                                                , SessionRepo
                                                , VerificationCode
                                                , getUser
                                                , login
                                                , mkEmail
                                                , mkPassword
                                                , rawEmail
                                                , register
                                                , verifyEmail
                                                )
import           Katip                          ( KatipContext )
import           Network.HTTP.Types             ( status400 )
import qualified Text.Digestive                as DF
import           Text.Digestive.Form            ( (.:) )
import           Web.Scotty.Internal.Types      ( ScottyError(stringError)
                                                , ScottyT
                                                )
import           Web.Scotty.Trans               ( get
                                                , json
                                                , post
                                                , raise
                                                , status
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
    input        <- parseAndValidateJSON authForm
    domainResult <- lift $ register input
    case domainResult of
      Left RegistrationErrorEmailTaken -> do
        status status400
        json ("EmailTaken" :: Text)
      Right _ -> return ()
  post "/api/auth/verifyEmail" $ do
    input        <- parseAndValidateJSON verifyEmailForm
    domainResult <- lift $ verifyEmail input
    case domainResult of
      Left EmailVerificationErrorInvalidCode -> do
        status status400
        json ("InvalidCode" :: Text)
      Right _ -> return ()
  post "/api/auth/login" $ do
    input        <- parseAndValidateJSON authForm
    domainResult <- lift $ login input
    case domainResult of
      Left LoginErrorInvalidAuth -> do
        status status400
        json ("InvalidAuth" :: Text)
      Left LoginErrorEmailNotVerified -> do
        status status400
        json ("EmailNotVerified" :: Text)
      Right sessionId -> do
        setSessionIdInCookie sessionId
        return ()
  get "/api/users" $ do
    userId <- reqCurrentUserId
    mEmail <- lift $ getUser userId
    case mEmail of
      Nothing ->
        -- all valid users must have an email
        raise $ stringError "Should not happen: SessionId map to invalid UserId"
      Just email -> json $ rawEmail email

-- TODO handle this using specific error types: Domain.Auth.EmailValidationErr Domain.Auth.PasswordValidationErr
-- | Structure for login and registration endpoints
authForm :: (Monad m) => DF.Form [Text] m Auth
authForm = Auth <$> "email" .: emailForm <*> "password" .: passwordForm
 where
  emailForm    = DF.validate (toResult . asText . mkEmail) (DF.text Nothing)
  passwordForm = DF.validate (toResult . asText . mkPassword) (DF.text Nothing)
  asText :: (Show e) => Either [e] d -> Either [Text] d
  asText = left (pack . show <$>)

verifyEmailForm :: (Monad m) => DF.Form [Text] m VerificationCode
verifyEmailForm = DF.text Nothing
