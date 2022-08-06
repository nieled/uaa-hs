module Adapter.HTTP.Web.Main where

import qualified Adapter.HTTP.Web.Auth         as Auth
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
import           Network.Wai.Middleware.Gzip    ( GzipFiles(GzipCompress)
                                                , GzipSettings(gzipFiles)
                                                , def
                                                , gzip
                                                )
import           Network.Wai.Middleware.Static  ( CacheContainer
                                                , CachingStrategy
                                                  ( PublicStaticCaching
                                                  )
                                                , Options(cacheContainer)
                                                , addBase
                                                , initCaching
                                                , staticPolicy'
                                                )
import           Web.Scotty.Trans               ( ScottyError(..)
                                                , ScottyT
                                                , defaultHandler
                                                , get
                                                , middleware
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
main runner = do
  cacheContainer <- initCaching PublicStaticCaching
  scottyAppT runner $ routes cacheContainer

routes
  :: ( MonadIO m
     , KatipContext m
     , AuthRepo m
     , EmailVerificationNotif m
     , SessionRepo m
     )
  => CacheContainer
  -> ScottyT Text m ()
routes cachingStrategy = do
  middleware $ gzip $ def { gzipFiles = GzipCompress }
  middleware
    $ staticPolicy' cachingStrategy (addBase "src/Adapter/HTTP/Web/static")

  Auth.routes

  notFound $ do
    status status404
    text "Not found"

  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
    status status500
    text "Internal server error!"
