module Adapter.RabbitMQ.Common where

import           Control.Exception              ( bracket )
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
