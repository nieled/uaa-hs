module Domain.Validation where

import           Data.Maybe                     ( maybeToList )
import           Data.MonoTraversable           ( MonoFoldable(olength) )
import           Data.Text                      ( Text
                                                , length
                                                )
import           Text.Regex.TDFA                ( (=~) )

type Validation e a = a -> Maybe e

validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b
validate constructor validations val =
  case concatMap (\f -> maybeToList $ f val) validations of
    []   -> Right $ constructor val
    errs -> Left errs

rangeBetween :: (Ord a) => a -> a -> e -> Validation e a
rangeBetween minRange maxRange msg val =
  if val >= minRange && val <= maxRange then Nothing else Just msg

lengthBetween :: MonoFoldable a => Int -> Int -> e -> Validation e a
lengthBetween minLen maxLen msg val =
  rangeBetween minLen maxLen msg (olength val)

regexMatches :: Text -> e -> Validation e Text
regexMatches regex msg val = if val =~ regex then Nothing else Just msg
