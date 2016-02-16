module Knapsack where

import Dict exposing (Dict)
import PrioritySet exposing (PrioritySet)

type alias Priced s =
  { state : s
  , cost : Float
  }

-- keyFunc should produce a unique result for each state in seed because
-- there's no guarantee that the lower-cost state will be chosen in the case
-- of a conflict.
getKnapsacks :
  (s -> comparable) -> (s -> List (Priced s)) -> List (Priced s) ->
    Dict comparable (Priced s)
getKnapsacks keyFunc successorFunc seed =
  let
    keys = List.map (keyFunc << .state) seed
  in let
    states = Dict.fromList <| List.map2 (,) keys seed
    fringe = PrioritySet.fromList keys
  in
    knapsackHelper keyFunc successorFunc states fringe

knapsackHelper :
  (s -> comparable) -> (s -> List (Priced s)) -> Dict comparable (Priced s) ->
    PrioritySet comparable -> Dict comparable (Priced s)
knapsackHelper keyFunc successorFunc states fringe =
  case PrioritySet.findMin fringe of
    Nothing -> states
    Just key ->
      case Dict.get key states of
        Nothing -> Dict.empty -- this should never happen
        Just pricedState ->
          let
            successors =
              List.map
                (increaseCostBy pricedState.cost) <|
                successorFunc pricedState.state
          in let
            (newStates, newFringe) =
              List.foldl
                (insertState keyFunc)
                (states, PrioritySet.deleteMin fringe)
                successors
          in
            knapsackHelper keyFunc successorFunc newStates newFringe

increaseCostBy : Float -> Priced s -> Priced s
increaseCostBy increase pricedState =
  { pricedState | cost = pricedState.cost + increase }

insertState :
  (s -> comparable) -> Priced s ->
    (Dict comparable (Priced s), PrioritySet comparable) ->
    (Dict comparable (Priced s), PrioritySet comparable)
insertState keyFunc pricedState (states, fringe) =
  let
    key = keyFunc pricedState.state
  in let
    shouldInsert =
      Maybe.withDefault
        True <|
        Maybe.map ((<) pricedState.cost << .cost) <| Dict.get key states
  in
    if shouldInsert then
      ( Dict.insert key pricedState states , PrioritySet.insert key fringe )
    else (states, fringe)
