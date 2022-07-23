module Adapter.HTTP.Common where

import           Blaze.ByteString.Builder       ( toLazyByteString )
import           Control.Monad.Reader           ( MonadIO(liftIO)
                                                , MonadTrans(lift)
                                                )
import           Data.Aeson                     ( ToJSON
                                                , Value(Null)
                                                )
import           Data.Text                      ( Text )
import           Data.Text.Encoding            as T
                                                ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Text.Lazy                as LT
                                                ( toStrict )
import           Data.Text.Lazy.Encoding       as LT
                                                ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Time                      ( getCurrentTime )
import           Data.Time.Lens                 ( modL
                                                , month
                                                )
import           Domain.Auth                    ( SessionId
                                                , SessionRepo
                                                , UserId
                                                , resolveSessionId
                                                )
import           Network.HTTP.Types.Status      ( status401 )
import           Text.Digestive.Form           as DF
                                                ( Form )
import           Text.Digestive.Types          as DF
                                                ( Result(..) )
import           Web.Cookie                     ( SetCookie(..)
                                                , def
                                                , parseCookies
                                                , renderSetCookie
                                                , sameSiteLax
                                                )
import           Web.Scotty.Trans               ( ActionT
                                                , ScottyError
                                                , finish
                                                , header
                                                , json
                                                , setHeader
                                                , status
                                                )

-- | Parse the JSON and run the form, if the validation fails we return 400 err
-- with the corresponding error messages
parseAndValidateJSON
  :: (ScottyError e, MonadIO m, ToJSON v) => DF.Form v m a -> ActionT e m a
parseAndValidateJSON form = undefined

toResult :: Either e a -> DF.Result e a
toResult = either DF.Error DF.Success

setCookie :: (ScottyError e, Monad m) => SetCookie -> ActionT e m ()
setCookie =
  setHeader "Set-Cookie" . LT.decodeUtf8 . toLazyByteString . renderSetCookie

getCookie :: (ScottyError e, Monad m) => Text -> ActionT e m (Maybe Text)
getCookie key = do
  mCookieStr <- header "Cookie"
  return $ do
    cookie <- parseCookies . T.encodeUtf8 . toStrict <$> mCookieStr
    let bsKey = T.encodeUtf8 key
    val <- lookup bsKey cookie
    return $ T.decodeUtf8 val

setSessionIdInCookie
  :: (MonadIO m, ScottyError e) => SessionId -> ActionT e m ()
setSessionIdInCookie sessionId = do
  currentTime <- liftIO getCurrentTime
  setCookie $ def { setCookieName     = "sId"
                  , setCookiePath     = Just "/"
                  , setCookieValue    = T.encodeUtf8 sessionId
                  , setCookieExpires  = Just $ modL month (+ 1) currentTime
                  , setCookieHttpOnly = True
                  , setCookieSecure   = False
                  , setCookieSameSite = Just sameSiteLax
                  }

getCurrentUserId
  :: (SessionRepo m, ScottyError e) => ActionT e m (Maybe UserId)
getCurrentUserId = do
  mSessionId <- getCookie "sId"
  case mSessionId of
    Nothing        -> return Nothing
    Just sessionId -> lift $ resolveSessionId sessionId

reqCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m UserId
reqCurrentUserId = do
  mUserId <- getCurrentUserId
  case mUserId of
    Nothing -> do
      status status401
      json ("AuthRequired" :: Text)
      finish
    Just userId -> return userId
