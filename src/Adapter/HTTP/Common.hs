module Adapter.HTTP.Common where

import           Control.Monad.Reader           ( MonadIO
                                                , MonadTrans(lift)
                                                )
import           Data.Aeson                     ( ToJSON
                                                , Value(Null)
                                                )
import           Data.Text                      ( Text )
import           Domain.Auth                    ( SessionId
                                                , SessionRepo
                                                , UserId
                                                )
import           Text.Digestive.Form           as DF
                                                ( Form )
import           Text.Digestive.Types          as DF
                                                ( Result(..) )
import           Web.Cookie                     ( SetCookie )
import           Web.Scotty.Trans               ( ActionT
                                                , ScottyError
                                                , jsonData
                                                , rescue
                                                )

-- | Parse the JSON and run the form, if the validation fails we return 400 err
-- with the corresponding error messages
parseAndValidateJSON
  :: (ScottyError e, MonadIO m, ToJSON v) => DF.Form v m a -> ActionT e m a
parseAndValidateJSON form = undefined

toResult :: Either e a -> DF.Result e a
toResult = either DF.Error DF.Success

setCookie :: (ScottyError e, Monad m) => SetCookie -> ActionT e m ()
setCookie = undefined

getCookie :: (ScottyError e, Monad m) => Text -> ActionT e m (Maybe Text)
getCookie = undefined

setSessionIdInCookie
  :: (MonadIO m, ScottyError e) => SessionId -> ActionT e m ()
setSessionIdInCookie = undefined

getCurrentUserId
  :: (SessionRepo m, ScottyError e) => ActionT e m (Maybe UserId)
getCurrentUserId = undefined

reqCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m UserId
reqCurrentUserId = undefined
