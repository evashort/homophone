module Deletions where

import Dict
import String

import CompletionDict exposing (CompletionDict)
import DAG exposing (DAG, Edge)
import DeletionCosts exposing (DeletionCosts)
import Knapsack exposing (Priced, Knapsack)

getDeletions : DeletionCosts -> DAG -> Int -> (List (Priced Int), Int)
getDeletions deletionCosts dag i =
  let knapsacks =
    Dict.values <|
      Knapsack.getKnapsacks
        identity
        (deletionChoices deletionCosts dag)
        [ { state = i, cost = 0.0 } ]
  in
    ( List.map orphan knapsacks
    , Maybe.withDefault 0 <| List.maximum <| List.map .roadblock knapsacks
    )


orphan : Knapsack Int -> Priced Int
orphan knapsack =
  { state = knapsack.state
  , cost = knapsack.cost
  }

deletionChoices :
  DeletionCosts -> DAG -> Int -> (List (Priced Int), Int)
deletionChoices deletionCosts dag i =
  List.foldl
    (deletionChoicesHelper deletionCosts dag "")
    ([], 0) <|
    DAG.get i dag

deletionChoicesHelper :
  DeletionCosts -> DAG -> String -> Edge -> (List (Priced Int), Int) ->
    (List (Priced Int), Int)
deletionChoicesHelper deletionCosts dag key edge (choices, roadblock) =
  let
    newKey = key ++ String.fromChar edge.phoneme
  in let
    newChoices =
      case CompletionDict.get newKey deletionCosts of
        Nothing -> choices
        Just cost -> { state = edge.dst, cost = cost } :: choices
  in
    if CompletionDict.startWith newKey deletionCosts then
      List.foldl
        (deletionChoicesHelper deletionCosts dag newKey)
        (newChoices, roadblock) <|
        DAG.get edge.dst dag
    else (newChoices, max roadblock edge.dst)
