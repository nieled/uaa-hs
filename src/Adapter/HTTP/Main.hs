module Adapter.HTTP.Main where

import qualified Adapter.HTTP.API.Auth         as AuthAPI
import qualified Adapter.HTTP.API.Main         as API
import qualified Adapter.HTTP.Web.Main         as Web
import           Control.Monad.Reader           ( MonadIO
                                                , MonadTrans(..)
                                                )
import           Data.MonoTraversable           ( headMay )
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
import           Network.Wai                    ( Request(pathInfo)
                                                , Response
                                                )
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.Gzip    ( GzipFiles(GzipCompress)
                                                , GzipSettings(gzipFiles)
                                                , def
                                                , gzip
                                                )
import           Network.Wai.Middleware.Vhost   ( vhost )
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
main port runner = do
  web <- Web.main runner
  api <- API.main runner
  run port $ vhost [(pathBeginsWith "api", api)] web
  where pathBeginsWith path req = headMay (pathInfo req) == Just path
