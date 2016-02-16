module PricedValue where

import List
import String

blackCost : Float
blackCost = 0.0

grayCost : Float
grayCost = 1.0

whiteCost : Float
whiteCost = 2.0

type alias PricedValue = List (Char, Float)

type alias ValueSplit =
  { used : String
  , leftovers : PricedValue
  , cost : Float
  }

price : String -> List Int -> String -> PricedValue
price key spaces value =
  let
    keyLength = String.length key
    valueLength = String.length value
  in let
    costTiers =
      case (List.head spaces, List.head <| List.reverse spaces) of
        (Just firstSpace, Just lastSpace) ->
          [ (lastSpace + valueLength - keyLength, blackCost)
          , (valueLength - 1, whiteCost)
          , (firstSpace - 1, grayCost)
          ]
        _ -> []
  in
    List.indexedMap (pricePhoneme costTiers) <| String.toList value

pricePhoneme : List (Int, Float) -> Int -> Char -> (Char, Float)
pricePhoneme costTiers index phoneme =
  (phoneme, indexCost costTiers index)

indexCost : List (Int, Float) -> Int -> Float
indexCost costTiers index =
  case (List.head costTiers, List.tail costTiers) of
    (Just (start, cost), Just rest) ->
      if index >= start then cost else indexCost rest index
    _ -> blackCost

empty : PricedValue
empty = []

getValue : PricedValue -> String
getValue pricedValue = String.fromList <| List.map fst pricedValue

getValueSplits : PricedValue -> List ValueSplit
getValueSplits pricedValue =
  case (List.head pricedValue, List.tail pricedValue) of
    (Just first, Just rest) ->
      List.map3
        toValueSplit
        ( List.map
            (String.fromList << ((::) <| fst first) << List.reverse) <|
            List.scanl (::) [] <| List.map fst rest
        )
        (List.reverse <| List.scanl (::) [] <| List.reverse rest) <|
        List.map snd pricedValue
    _ -> []

toValueSplit : String -> PricedValue -> Float -> ValueSplit
toValueSplit used leftovers cost =
  { used = used, leftovers = leftovers, cost = cost }
