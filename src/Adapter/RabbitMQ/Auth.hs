module Adapter.RabbitMQ.Auth where

import qualified Adapter.InMemory.Auth         as M
import           Adapter.RabbitMQ.Common        ( Rabbit
                                                , State
                                                , consumeAndProcess
                                                , initConsumer
                                                , initQueue
                                                , publish
                                                , withMsgAndErr
                                                )
import           Control.Exception.Safe         ( MonadCatch )
import           Data.Aeson                     ( Options(fieldLabelModifier)
                                                , defaultOptions
                                                )
import           Data.Aeson.TH                  ( deriveJSON )
import           Data.Char                      ( isUpper
                                                , toLower
                                                )
import           Data.Text                      ( Text )
import qualified Domain.Auth                   as D
import           Katip                          ( KatipContext
                                                , Severity(ErrorS)
                                                , logTM
                                                )
import           Network.AMQP                   ( Message )

-- TODO: It stores the message in an in-memory database for now
-- Implement email sender adapter

-- | Message payload
data EmailVerificationPayload = EmailVerificationPayload
  { emailVerificationPayloadEmail            :: Text
  , emailVerificationPayloadVerificationCode :: Text
  }

$(let options = defaultOptions
                  { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 24
                  }
  in deriveJSON options ''EmailVerificationPayload)

-- | Sets up the necessary network topology and listener.
init
  :: (M.InMemory r m, KatipContext m, MonadCatch m)
  => State
  -> (m Bool -> IO Bool)
  -> IO ()
init state runner = do
  initQueue state "verifyEmail" "auth" "userRegistered"
  initConsumer state "verifyEmail" (consumeEmailVerification runner)

consumeEmailVerification
  :: (M.InMemory r m, KatipContext m, MonadCatch m)
  => (m Bool -> IO Bool)
  -> Message
  -> IO Bool
consumeEmailVerification runner message = runner
  $ consumeAndProcess message handler
 where
  handler payload = case D.mkEmail (emailVerificationPayloadEmail payload) of
    Left error -> withMsgAndErr message error $ do
      $(logTM) ErrorS "Email format is invalid. Rejecting."
      return False
    Right email -> do
      let vCode = emailVerificationPayloadVerificationCode payload
      M.notifyEmailVerification email vCode
      return True

-- | Publish the message to the “auth” exchange while using “userRegistered” as the routing key
notifyEmailVerification
  :: (Rabbit r m) => D.Email -> D.VerificationCode -> m ()
notifyEmailVerification email verificationCode =
  let payload = EmailVerificationPayload (D.rawEmail email) verificationCode
  in  publish "auth" "userRegistered" payload
