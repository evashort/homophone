module CostPair where

import String

import Parser

costMultiplier : Float
costMultiplier = 0.001

type alias CostPair = (String, Float)

parse : String -> Maybe CostPair
parse costPairString =
  case Parser.split2 "=" costPairString of
    Nothing -> Nothing
    Just ("", costString) -> Nothing
    Just (key, costString) ->
      Maybe.map
        ((,) key << (*) costMultiplier) <|
        Result.toMaybe <| String.toFloat costString
