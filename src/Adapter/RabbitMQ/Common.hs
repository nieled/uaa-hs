module Adapter.RabbitMQ.Common where

import           Control.Concurrent             ( forkIO )
import           Control.Exception              ( bracket )
import           Control.Exception.Safe         ( tryAny )
import           Control.Monad                  ( void )
import           Control.Monad.Catch            ( MonadCatch
                                                , displayException
                                                )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Data.Aeson
import           Data.Has
import           Data.Text                      ( Text )
import           Katip
import           Network.AMQP

data State = State
  { statePublisherChannel :: Channel
  , stateConsumerChannel  :: Channel
  }

-- | Constraint synonym
type Rabbit r m = (Has State r, MonadReader r m, MonadIO m)

-- | Initialize RabbitMQ state, do the action, and destroy the state.
withState
  :: String
  -- ^ Connection URL
  -> Integer
  -- ^ Prefetch count (Maximum number of messages to be received without confirmation)
  -> (State -> IO a)
  -- ^ Action to be carried out having constructed the state
  -> IO a
withState connectionUri prefetchCount action = bracket initState
                                                       destroyState
                                                       action'
 where
  initState = do
    -- It’s considered a best practice to separate the connection between publisher and consumer
    publisher <- openConnectionAndChannel
    consumer  <- openConnectionAndChannel
    return (publisher, consumer)
  openConnectionAndChannel = do
    connection <- openConnection'' . fromURI $ connectionUri
    channel    <- openChannel connection
    confirmSelect channel False
    qos channel 0 (fromInteger prefetchCount) True
    return (connection, channel)
  destroyState ((connection1, _), (connection2, _)) = do
    closeConnection connection1
    closeConnection connection2
  action' ((_, publisherChannel), (_, consumerChannel)) =
    action (State publisherChannel consumerChannel)

-- | Create a exchange hard-coded as "topic"
-- Does not matter if using publisher or consumer to open the exchange
initExchange :: State -> Text -> IO ()
initExchange (State publisherChannel _) exchangeName = do
  let exchange =
        newExchange { exchangeName = exchangeName, exchangeType = "topic" }
  declareExchange publisherChannel exchange

-- | Create a queue along with an exchange and the binding
-- NOTE: Declaring an exchange is an idempotent operation
initQueue :: State -> Text -> Text -> Text -> IO ()
initQueue state queueName exchangeName routingKey = do
  initExchange state exchangeName
  void $ declareQueue (statePublisherChannel state)
                      (newQueue { queueName = queueName })
  bindQueue (statePublisherChannel state) queueName exchangeName routingKey

-- | Process the RabbitMQ Message and return True if the message is processed successfully and False otherwise
initConsumer :: State -> Text -> (Message -> IO Bool) -> IO ()
initConsumer state queueName handler = do
  void
    . consumeMsgs (stateConsumerChannel state) queueName Ack
    $ \(message, env) -> void . forkIO $ do
        result <- handler message
        if result then ackEnv env else rejectEnv env False

-- | Send any data to RabbitMQ, provided the data can be serialized to JSON
-- It gets a publisher channel from the environment, constructs the payload, and sends it to RabbitMQ
publish :: (ToJSON a, Rabbit r m) => Text -> Text -> a -> m ()
publish exchange routingKey payload = do
  (State channel _) <- asks getter
  let msg = newMsg { msgBody = encode payload }
  liftIO . void $ publishMsg channel exchange routingKey msg

-- | Consume RabbitMQ messages. We expect to receive a JSON formatted value
consumeAndProcess
  :: (KatipContext m, FromJSON a, MonadCatch m)
  => Message
  -> (a -> m Bool)
  -> m Bool
consumeAndProcess message handler = case eitherDecode' (msgBody message) of
  Left error -> withMsgAndErr message error $ do
    $(logTM) ErrorS "Malformed payload. Rejecting."
    return False
  Right payload -> do
    -- Catch any synchronous exceptions. `tryAny` operates under the MonadCatch typeclass
    result <- tryAny (handler payload)
    case result of
      Left error -> withMsgAndErr message (displayException error) $ do
        $(logTM) ErrorS
                 "There was an exception when processing the message. Rejecting."
        return False
      Right bool -> return bool

withMsgAndErr :: (KatipContext m, ToJSON e) => Message -> e -> m a -> m a
withMsgAndErr message error =
  katipAddContext (sl "mqMsg" (show message) <> sl "error" error)
