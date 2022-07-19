module Adapter.RabbitMQ.Common where

import           Control.Exception              ( bracket )
import           Network.AMQP

data State = State
  { statePublisherChannel :: Channel
  , stateConsumerChannel  :: Channel
  }

withState :: String -> Integer -> (State -> IO a) -> IO a
withState connectionUri prefetchCount action = bracket initState
                                                       destroyState
                                                       action'
 where
  initState = do
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
  action' ((_, pubChannel), (_, conChannel)) =
    action (State pubChannel conChannel)
