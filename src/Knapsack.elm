module Knapsack where

import Dict exposing (Dict)
import Random

import PeakedList exposing (PeakedList)
import PrioritySet exposing (PrioritySet)

type alias Priced s =
  { state : s
  , cost : Float
  }

type alias Knapsack s =
  { state : s
  , ancestors : List s
  , cost : Float
  , peak : Int
  }

emptyCache : (s -> comparable) -> s -> Dict comparable (Knapsack s)
emptyCache keyFunc state =
  Dict.singleton
    (keyFunc state)
    { state = state, ancestors = [], cost = 0.0, peak = Random.maxInt }

mapPeaks :
  (Int -> Int) -> Dict comparable (Knapsack s) -> Dict comparable (Knapsack s)
mapPeaks f cache = Dict.map (curry <| mapPeak f << snd) cache

mapPeak : (Int -> Int) -> Knapsack s -> Knapsack s
mapPeak f knapsack = { knapsack | peak = f knapsack.peak }

-- keyFunc should produce a unique result for each state in seed because
-- there's no guarantee that the lower-cost state will be chosen in the case
-- of a conflict.
getKnapsacks :
  (s -> comparable) -> (comparable -> PeakedList (Priced s)) ->
    Dict comparable (Knapsack s) -> Int ->
    (Dict comparable (Knapsack s), Bool)
getKnapsacks keyFunc successorFunc cache maxIterations =
  let
    fringe =
      PrioritySet.fromList <|
        Dict.keys <|
          Dict.filter (curry <| (==) Random.maxInt << .peak << snd) cache
  in
    knapsackHelper keyFunc successorFunc cache fringe maxIterations

toChild : List s -> Float -> Priced s -> Knapsack s
toChild ancestors parentCost pricedState =
  { state = pricedState.state
  , ancestors = ancestors
  , cost = parentCost + pricedState.cost
  , peak = Random.maxInt
  }

knapsackHelper :
  (s -> comparable) -> (comparable -> PeakedList (Priced s)) ->
    Dict comparable (Knapsack s) -> PrioritySet comparable -> Int ->
    (Dict comparable (Knapsack s), Bool)
knapsackHelper keyFunc successorFunc knapsacks fringe iterations =
  if iterations <= 0 then (knapsacks, False)
  else
    case PrioritySet.findMin fringe of
      Nothing -> (knapsacks, True)
      Just key ->
        case Dict.get key knapsacks of
          Nothing -> Debug.crash "fringe key not found in knapsacks"
          Just knapsack ->
            let
              ancestors = knapsack.state :: knapsack.ancestors
              peakedSuccessors = successorFunc key
            in let
              successors =
                List.map
                  (toChild ancestors knapsack.cost)
                  peakedSuccessors.list
              raisedKnapsacks =
                Dict.insert
                  key
                  { knapsack | peak = peakedSuccessors.peak }
                  knapsacks
            in let
              (newKnapsacks, newFringe) =
                List.foldl
                  (insertKnapsack keyFunc)
                  (raisedKnapsacks, PrioritySet.deleteMin fringe)
                  successors
            in
              knapsackHelper
                keyFunc successorFunc newKnapsacks newFringe (iterations - 1)

insertKnapsack :
  (s -> comparable) -> Knapsack s ->
    (Dict comparable (Knapsack s), PrioritySet comparable) ->
    (Dict comparable (Knapsack s), PrioritySet comparable)
insertKnapsack keyFunc knapsack (knapsacks, fringe) =
  let
    key = keyFunc knapsack.state
  in let
    shouldInsert =
      Maybe.withDefault
        True <|
        Maybe.map ((<) knapsack.cost << .cost) <| Dict.get key knapsacks
  in
    if shouldInsert then
      ( Dict.insert key knapsack knapsacks, PrioritySet.insert key fringe )
    else (knapsacks, fringe)
