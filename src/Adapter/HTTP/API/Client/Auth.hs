module Adapter.HTTP.API.Client.Auth where

import           Adapter.HTTP.API.Client.Common
import           Adapter.HTTP.API.Types.Auth    ( )
import           Data.Aeson
import           Data.Has
import qualified Domain.Auth                   as D
import           Network.HTTP.Client
import           Network.HTTP.Types

register :: HttpClient r m => D.Auth -> m (Either D.RegistrationError ())
register auth = undefined

verifyEmail
  :: HttpClient r m
  => D.VerificationCode
  -> m (Either D.EmailVerificationError ())
verifyEmail code = undefined

login :: HttpClient r m => D.Auth -> m (Either D.LoginError Session)
login auth = undefined

getUser :: HttpClient r m => Session -> m D.Email
getUser session = undefined
