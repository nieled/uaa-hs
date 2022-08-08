{-# LANGUAGE OverloadedStrings #-}
module Adapter.HTTP.API.Types.AesonHelper where

import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Text
import           Domain.Auth                    ( LoginError )
import           Language.Haskell.TH.Syntax
import           Prelude                 hiding ( drop
                                                , length
                                                , map
                                                )

newtype Email
  = Email { rawEmail :: Text }
  deriving (Show, Eq, Ord)
newtype Password
  = Password { rawPassword :: Text }
  deriving (Show, Eq)

data Auth = Auth
  { authEmail    :: Email
  , authPassword :: Password
  }
  deriving (Eq, Show)
data LoginError
  = LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Eq, Show)

-- TODO: Fix OverloadedStrings issues

withSmartConstructor :: (a -> Either [Text] b) -> a -> Parser b
withSmartConstructor constructor a = case constructor a of
  Left  errs  -> fail . unpack . intercalate ". " $ errs
  Right value -> return value

deriveJSONRecord :: Name -> Q [Dec]
deriveJSONRecord record =
  let lowerCaseFirst (y : ys) = undefined --toLower [y] <> ys
      lowerCaseFirst ""       = ""
      structName = nameBase record
      opts       = defaultOptions { fieldLabelModifier = undefined }-- lowerCaseFirst . drop (length structName)
  in  deriveJSON opts record

deriveJSONSumType :: Name -> Q [Dec]
deriveJSONSumType record =
  let structName = nameBase record
      opts       = defaultOptions { constructorTagModifier = undefined -- drop (length structName)
                                  , tagSingleConstructors  = True
                                  }
  in  deriveJSON opts record

deriveToJSONUnwrap :: Name -> Q [Dec]
deriveToJSONUnwrap =
  let opts = defaultOptions { unwrapUnaryRecords = True } in deriveToJSON opts
