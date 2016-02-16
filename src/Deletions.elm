module Deletions where

import Dict
import String

import CompletionDict exposing (CompletionDict)
import DAG exposing (DAG, Edge)
import DeletionCosts exposing (DeletionCosts)
import Knapsack exposing (Priced)

type alias Hiker =
  { i : Int
  , inSpace : Bool
  }

getDeletions : DeletionCosts -> DAG -> Int -> List (Priced Hiker)
getDeletions deletionCosts dag i =
  List.map
    (toHiker dag i) <|
    Dict.values <|
      Knapsack.getKnapsacks
        identity
        (deletionChoices deletionCosts dag)
        [ { state = i, cost = 0.0 } ]

toHiker : DAG -> Int -> Priced Int -> Priced Hiker
toHiker dag start deletion =
  { deletion
  | state =
      { i = deletion.state
      , inSpace = DAG.spaceInRange start deletion.state dag
      }
  }

deletionChoices :
  DeletionCosts -> DAG -> Int -> List (Priced Int)
deletionChoices deletionCosts dag i =
  List.concatMap
    (deletionChoicesHelper deletionCosts dag "") <|
    DAG.get i dag

deletionChoicesHelper :
  DeletionCosts -> DAG -> String -> Edge -> List (Priced Int)
deletionChoicesHelper deletionCosts dag key edge =
  let
    newKey = key ++ String.fromChar edge.phoneme
  in let
    rest =
      if CompletionDict.startWith newKey deletionCosts then
        List.concatMap
          (deletionChoicesHelper deletionCosts dag newKey) <|
          DAG.get edge.dst dag
      else []
  in
    case CompletionDict.get newKey deletionCosts of
      Nothing -> rest
      Just cost -> { state = edge.dst, cost = cost } :: rest
