module Knapsack exposing (..)

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

mapPeaks :
  (Int -> Int) -> Dict comparable (Knapsack s) -> Dict comparable (Knapsack s)
mapPeaks f knapsacks = Dict.map (curry <| mapPeak f << snd) knapsacks

mapPeak : (Int -> Int) -> Knapsack s -> Knapsack s
mapPeak f knapsack = { knapsack | peak = f knapsack.peak }

type alias Search comparable s =
  { knapsacks : Dict comparable (Knapsack s)
  , fringe : PrioritySet comparable
  , keyFunc : s -> comparable
  , successorFunc : comparable -> PeakedList (Priced s)
  }

singleton :
  (s -> comparable) -> (comparable -> PeakedList (Priced s)) -> s ->
    Search comparable s
singleton keyFunc successorFunc state =
  let key = keyFunc state in
    { knapsacks =
        Dict.singleton
          key
          { state = state, ancestors = [], cost = 0.0, peak = Random.maxInt }
    , fringe = PrioritySet.singleton key
    , keyFunc = keyFunc
    , successorFunc = successorFunc
    }

init :
  (s -> comparable) -> (comparable -> PeakedList (Priced s)) ->
    Dict comparable (Knapsack s) -> Search comparable s
init keyFunc successorFunc knapsacks =
  { knapsacks = knapsacks
  , fringe =
      PrioritySet.fromList <|
        Dict.keys <|
          Dict.filter (curry <| (==) Random.maxInt << .peak << snd) knapsacks
  , keyFunc = keyFunc
  , successorFunc = successorFunc
  }

knapsacks : Search comparable s -> Dict comparable (Knapsack s)
knapsacks = .knapsacks

done : Search comparable s -> Bool
done search = PrioritySet.isEmpty search.fringe

update : Int -> Search comparable s -> (Search comparable s, Int)
update iterations search =
  if iterations <= 0 then (search, iterations)
  else
    case PrioritySet.findMin search.fringe of
      Nothing -> (search, iterations)
      Just key ->
        case Dict.get key search.knapsacks of
          Nothing -> Debug.crash "fringe key not found in knapsacks"
          Just knapsack ->
            let
              ancestors = knapsack.state :: knapsack.ancestors
              peakedSuccessors = search.successorFunc key
            in let
              successors =
                List.map
                  (toChild ancestors knapsack.cost)
                  peakedSuccessors.list
              raisedKnapsacks =
                Dict.insert
                  key
                  { knapsack | peak = peakedSuccessors.peak }
                  search.knapsacks
            in
              update
                (iterations - 1) <|
                List.foldl
                  insertKnapsack
                  { search
                  | fringe = PrioritySet.deleteMin search.fringe
                  , knapsacks = raisedKnapsacks
                  }
                  successors

toChild : List s -> Float -> Priced s -> Knapsack s
toChild ancestors parentCost pricedState =
  { state = pricedState.state
  , ancestors = ancestors
  , cost = parentCost + pricedState.cost
  , peak = Random.maxInt
  }

insertKnapsack : Knapsack s -> Search comparable s -> Search comparable s
insertKnapsack knapsack search =
  let
    key = search.keyFunc knapsack.state
  in let
    shouldInsert =
      Maybe.withDefault
        True <|
        Maybe.map
          ((<) knapsack.cost << .cost) <|
          Dict.get key search.knapsacks
  in
    if shouldInsert then
      { search
      | fringe = PrioritySet.insert key search.fringe
      , knapsacks = Dict.insert key knapsack search.knapsacks
      }
    else search
