module Adapter.RabbitMQ.Auth where

import qualified Adapter.InMemory.Auth         as M
import           Adapter.RabbitMQ.Common
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char                      ( isUpper
                                                , toLower
                                                )
import           Data.Text                      ( Text )
import qualified Domain.Auth                   as D
import           Katip
import           Network.AMQP

-- TODO: It stores the message in an in-memory database for now
-- Implement email sender adapter

-- | Message payload
data EmailVerificationPayload = EmailVerificationPayload
  { emailVerificationPayloadEmail            :: Text
  , emailVerificationPayloadVerificationCode :: Text
  }
  deriving Show

-- | Make it serializable and deserializable to JSON
$(let options = defaultOptions
                  { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 24 -- TODO: fixme
                  }
  in deriveJSON options ''EmailVerificationPayload)
