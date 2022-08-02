module Adapter.HTTP.API.Common where

import           Adapter.HTTP.Common            ( getCurrentUserId )
import           Blaze.ByteString.Builder       ( toLazyByteString )
import           Control.Monad.Reader           ( MonadIO(liftIO)
                                                , MonadTrans(lift)
                                                )
import           Data.Aeson                     ( (.=)
                                                , ToJSON
                                                , Value(Null)
                                                , object
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
import           Network.HTTP.Types.Status      ( status400
                                                , status401
                                                )
import           Text.Digestive                 ( View )
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
                                                , jsonData
                                                , rescue
                                                , setHeader
                                                , status
                                                )

-- | Parse the JSON and run the form, if the validation fails we return 400 err
-- with the corresponding error messages
parseAndValidateJSON
  :: (ScottyError e, MonadIO m, ToJSON v) => DF.Form v m a -> ActionT e m a
parseAndValidateJSON form = do
  val              <- jsonData `rescue` (\_ -> return Null)
  validationResult <- lift $ validate form val
  case validationResult of
    (v, Nothing) -> do
      status status400
      json $ handleJSONErrors v
      finish
    (_, Just result) -> do
      return result
 where
  -- | Given a JSON document and a form, attempt to use the JSON document to evaluation the form. If the form fails validation, then 'Nothing' is returned.
  validate :: (Monad m) => Form v m a -> Value -> m (View v, Maybe a)
  validate form json = undefined
  -- | Takes a 'View' and displays any errors in a hierachical format that matches the expected input.
  handleJSONErrors :: ToJSON a => View a -> Value
  handleJSONErrors v = undefined

reqCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m UserId
reqCurrentUserId = do
  mUserId <- getCurrentUserId
  case mUserId of
    Nothing -> do
      status status401
      json $ errorResponse ("Auth Required" :: Text)
      finish
    Just userId -> return userId

errorResponse :: (ToJSON a) => a -> Value
errorResponse val = object ["error" .= val]
