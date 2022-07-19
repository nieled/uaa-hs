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
  initState                = undefined
  openConnectionAndChannel = undefined
  destroyState             = undefined
  action'                  = undefined

