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

getKnapsacks :
  (s -> comparable) -> (comparable -> PeakedList (Priced s)) -> Int ->
    Dict comparable (Knapsack s) -> (Dict comparable (Knapsack s), Int)
getKnapsacks keyFunc successorFunc iterations cache =
  let
    fringe =
      PrioritySet.fromList <|
        Dict.keys <|
          Dict.filter (curry <| (==) Random.maxInt << .peak << snd) cache
  in
    knapsackHelper keyFunc successorFunc iterations fringe cache

done : Dict comparable (Knapsack s) -> Bool
done = Dict.foldl alsoNotOnFringe True

alsoNotOnFringe : comparable -> Knapsack s -> Bool -> Bool
alsoNotOnFringe _ knapsack noneOnFringe =
  noneOnFringe && knapsack.peak /= Random.maxInt

toChild : List s -> Float -> Priced s -> Knapsack s
toChild ancestors parentCost pricedState =
  { state = pricedState.state
  , ancestors = ancestors
  , cost = parentCost + pricedState.cost
  , peak = Random.maxInt
  }

knapsackHelper :
  (s -> comparable) -> (comparable -> PeakedList (Priced s)) -> Int ->
    PrioritySet comparable -> Dict comparable (Knapsack s) ->
    (Dict comparable (Knapsack s), Int)
knapsackHelper keyFunc successorFunc iterations fringe knapsacks =
  if iterations <= 0 then (knapsacks, iterations)
  else
    case PrioritySet.findMin fringe of
      Nothing -> (knapsacks, iterations)
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
              (newFringe, newKnapsacks) =
                List.foldl
                  (insertKnapsack keyFunc)
                  (PrioritySet.deleteMin fringe, raisedKnapsacks)
                  successors
            in
              knapsackHelper
                keyFunc successorFunc (iterations - 1) newFringe newKnapsacks

insertKnapsack :
  (s -> comparable) -> Knapsack s ->
    (PrioritySet comparable, Dict comparable (Knapsack s)) ->
    (PrioritySet comparable, Dict comparable (Knapsack s))
insertKnapsack keyFunc knapsack (fringe, knapsacks) =
  let
    key = keyFunc knapsack.state
  in let
    shouldInsert =
      Maybe.withDefault
        True <|
        Maybe.map ((<) knapsack.cost << .cost) <| Dict.get key knapsacks
  in
    if shouldInsert then
      ( PrioritySet.insert key fringe, Dict.insert key knapsack knapsacks )
    else (fringe, knapsacks)
