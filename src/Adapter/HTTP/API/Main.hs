module Adapter.HTTP.API.Main where

import qualified Adapter.HTTP.API.Auth         as Auth
import           Adapter.HTTP.API.Common        ( errorResponse )
import           Control.Monad.Reader           ( MonadIO
                                                , MonadTrans(..)
                                                )
import           Data.Text.Lazy                 ( Text )
import           Domain.Auth                    ( AuthRepo
                                                , EmailVerificationNotif
                                                , SessionRepo
                                                )
import           Katip                          ( KatipContext
                                                , Severity(ErrorS)
                                                , logTM
                                                , ls
                                                )
import           Network.HTTP.Types.Status      ( status404
                                                , status500
                                                )
import           Network.Wai                    ( Application
                                                , Response
                                                )
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
                                                , notFound
                                                , scottyAppT
                                                , status
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
  middleware $ gzip $ def { gzipFiles = GzipCompress }

  Auth.routes

  notFound $ do
    status status404
    json $ errorResponse ("NotFound" :: Text)

  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
    status status500
    json $ errorResponse ("InternalServerError" :: Text)
