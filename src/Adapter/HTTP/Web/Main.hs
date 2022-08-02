module Adapter.HTTP.Web.Main where

import           Control.Monad.Reader           ( MonadIO
                                                , MonadTrans(..)
                                                )
import           Data.Text.Lazy                 ( Text )
import           Domain.Auth                    ( AuthRepo
                                                , EmailVerificationNotif
                                                , SessionRepo
                                                )
import           Katip                          ( KatipContext
                                                , Severity(..)
                                                , logTM
                                                , ls
                                                )
import           Network.HTTP.Types             ( status404
                                                , status500
                                                )
import           Network.Wai                    ( Application
                                                , Request(pathInfo)
                                                , Response
                                                )
import           Web.Scotty.Trans               ( ScottyError(..)
                                                , ScottyT
                                                , defaultHandler
                                                , get
                                                , notFound
                                                , scottyAppT
                                                , status
                                                , text
                                                )
main
  :: ( MonadIO m
     , KatipContext m
     , AuthRepo m
     , EmailVerificationNotif m
     , SessionRepo m
     )
  => (m Response -> IO Response)
  -> IO Application
main runner = scottyAppT runner routes

routes
  :: ( MonadIO m
     , KatipContext m
     , AuthRepo m
     , EmailVerificationNotif m
     , SessionRepo m
     )
  => ScottyT Text m ()
routes = do
  get "/" $ text "Hello from web!"

  notFound $ do
    status status404
    text "Not found"

  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
    status status500
    text "Internal server error!"
