module Adapter.HTTP.Web.Auth where

import           Adapter.HTTP.Common
import           Adapter.HTTP.Web.Common
import           Control.Monad.Reader           ( MonadIO )
import           Domain.Auth
import           Katip
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

  get "/auth/verifyEmail/:code" $ undefined

  get "/auth/login" $ undefined
  post "/auth/login" $ undefined

  get "/users" $ undefined
