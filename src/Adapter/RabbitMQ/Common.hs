module Adapter.RabbitMQ.Common where

import           Control.Concurrent             ( forkIO )
import           Control.Exception              ( bracket )
import           Control.Monad                  ( void )
import           Data.Text                      ( Text )
import           Network.AMQP

data State = State
  { statePublisherChannel :: Channel
  , stateConsumerChannel  :: Channel
  }

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

initConsumer :: State -> Text -> (Message -> IO Bool) -> IO ()
initConsumer state queueName handler = do
  void
    . consumeMsgs (stateConsumerChannel state) queueName Ack
    $ \(message, env) -> void . forkIO $ do
        result <- handler message
        if result then ackEnv env else rejectEnv env False
