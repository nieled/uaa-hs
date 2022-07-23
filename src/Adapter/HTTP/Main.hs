module Adapter.HTTP.Main where

import qualified Adapter.HTTP.API.Auth         as AuthAPI
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
import           Network.HTTP.Types             ( status500 )
import           Network.Wai                    ( Response )
import           Network.Wai.Middleware.Gzip    ( GzipFiles(GzipCompress)
                                                , GzipSettings(gzipFiles)
                                                , def
                                                , gzip
                                                )
import           Web.Scotty.Trans               ( ScottyError(showError)
                                                , ScottyT
                                                , defaultHandler
                                                , json
                                                , middleware
                                                , scottyT
                                                , status
                                                )

main
  :: ( MonadIO m
     , KatipContext m
     , AuthRepo m
     , EmailVerificationNotif m
     , SessionRepo m
     )
  => Int
  -> (m Response -> IO Response)
  -> IO ()
main port runner = scottyT port runner routes

routes
  :: ( MonadIO m
     , KatipContext m
     , AuthRepo m
     , EmailVerificationNotif m
     , SessionRepo m
     )
  => ScottyT Text m ()
routes = do
  middleware $ gzip $ def { gzipFiles = GzipCompress }
  AuthAPI.routes
  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
    status status500
    json ("Internal Server Error" :: Text)
