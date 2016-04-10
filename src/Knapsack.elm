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

toRoot : Priced s -> Knapsack s
toRoot pricedState =
  { state = pricedState.state
  , ancestors = []
  , cost = pricedState.cost
  , peak = Random.maxInt
  }

-- keyFunc should produce a unique result for each state in seed because
-- there's no guarantee that the lower-cost state will be chosen in the case
-- of a conflict.
getKnapsacks :
  (s -> comparable) -> (comparable -> PeakedList (Priced s)) ->
    List (Knapsack s) -> Int -> Dict comparable (Knapsack s)
getKnapsacks keyFunc successorFunc cache seaLevel =
  let
    states =
      Dict.fromList <|
        List.map2 (,) (List.map (keyFunc << .state) cache) cache
    fringe =
      PrioritySet.fromList <|
        List.map
          (keyFunc << .state) <|
          List.filter ((<=) seaLevel << .peak) cache
  in
    knapsackHelper keyFunc successorFunc states fringe

toChild : List s -> Float -> Priced s -> Knapsack s
toChild ancestors parentCost pricedState =
  { state = pricedState.state
  , ancestors = ancestors
  , cost = parentCost + pricedState.cost
  , peak = Random.maxInt
  }

knapsackHelper :
  (s -> comparable) -> (comparable -> PeakedList (Priced s)) ->
    Dict comparable (Knapsack s) -> PrioritySet comparable ->
    Dict comparable (Knapsack s)
knapsackHelper keyFunc successorFunc knapsacks fringe =
  case PrioritySet.findMin fringe of
    Nothing -> knapsacks
    Just key ->
      case Dict.get key knapsacks of
        Nothing -> Dict.empty -- this should never happen
        Just knapsack ->
          let
            ancestors = knapsack.state :: knapsack.ancestors
            peakedSuccessors = successorFunc key
          in let
            successors =
              List.map (toChild ancestors knapsack.cost) peakedSuccessors.list
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
            knapsackHelper keyFunc successorFunc newKnapsacks newFringe

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
