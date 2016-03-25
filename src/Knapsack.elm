module Knapsack where

import Dict exposing (Dict)
import PrioritySet exposing (PrioritySet)

type alias Priced s =
  { state : s
  , cost : Float
  }

type alias Knapsack s =
  { state : s
  , ancestors : List s
  , cost : Float
  , roadblock : Int
  }

-- keyFunc should produce a unique result for each state in seed because
-- there's no guarantee that the lower-cost state will be chosen in the case
-- of a conflict.
getKnapsacks :
  (s -> comparable) -> (comparable -> (List (Priced s), Int)) ->
    List (Knapsack s) -> Int -> List (Priced s) ->
    Dict comparable (Knapsack s)
getKnapsacks keyFunc successorFunc cache changeStart seed =
  let
    stateList = List.map (toChild [] 0.0) seed ++ cache
    seedKeys = List.map (keyFunc << .state) seed
    cacheKeys =
      List.map
        (keyFunc << .state) <|
        List.filter ((<=) changeStart << .roadblock) cache
  in let
    states =
      Dict.fromList <|
        List.map2 (,) (List.map (keyFunc << .state) stateList) stateList
    fringe = PrioritySet.fromList (seedKeys ++ cacheKeys)
  in
    knapsackHelper keyFunc successorFunc states fringe

toChild : List s -> Float -> Priced s -> Knapsack s
toChild ancestors parentCost pricedState =
  { state = pricedState.state
  , ancestors = ancestors
  , cost = parentCost + pricedState.cost
  , roadblock = 0
  }

knapsackHelper :
  (s -> comparable) -> (comparable -> (List (Priced s), Int)) ->
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
            successorsAndRoadblock = successorFunc key
          in let
            successors =
              List.map
                (toChild ancestors knapsack.cost) <|
                fst successorsAndRoadblock
            roadblock = snd successorsAndRoadblock
          in let
            knapsacksWithRoadblock =
              Dict.insert key { knapsack | roadblock = roadblock } knapsacks
          in let
            (newKnapsacks, newFringe) =
              List.foldl
                (insertKnapsack keyFunc)
                (knapsacksWithRoadblock, PrioritySet.deleteMin fringe)
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
