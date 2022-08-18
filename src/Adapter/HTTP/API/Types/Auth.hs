{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Adapter.HTTP.API.Types.Auth where

import           Adapter.HTTP.API.Types.AesonHelper
import           Data.Aeson
import           Domain.Auth             hiding ( Auth
                                                , Email
                                                , LoginError
                                                , Password
                                                )

instance FromJSON Email where
  parseJSON = withText "Email" $ withSmartConstructor mkEmail

instance FromJSON Password where
  parseJSON = withText "Password" $ withSmartConstructor mkPassword

$(map concat . sequence $
  [ deriveJSONRecord ''Auth
  , deriveToJSONUnwrap ''Email
  , deriveToJSONUnwrap ''Password
  , deriveJSONSumType ''RegistrationError
  , deriveJSONSumType ''EmailVerificationError
  , deriveJSONSumType ''LoginError
  ])
