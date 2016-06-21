module PricedString exposing (PricedString, parse)

import String

import ParseUtils

costMultiplier : Float
costMultiplier = 0.001

type alias PricedString = (String, Float)

parse : String -> Maybe PricedString
parse text =
  case ParseUtils.split2 "=" text of
    Nothing -> Nothing
    Just ("", costString) -> Nothing
    Just (string, costString) ->
      Maybe.map
        ((,) string << (*) costMultiplier) <|
        Result.toMaybe <| String.toFloat costString
