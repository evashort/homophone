module Deletions where

import Dict
import Random
import String

import CompletionDict exposing (CompletionDict)
import DAG exposing (DAG, Edge)
import DeletionCosts exposing (DeletionCosts)
import Knapsack exposing (Priced, Knapsack)
import PeakedList exposing (PeakedList)

getDeletions : DeletionCosts -> DAG -> Int -> PeakedList (Priced Int)
getDeletions deletionCosts dag i =
  let knapsacks =
    Dict.values <|
      Knapsack.getKnapsacks
        identity
        (deletionChoices deletionCosts dag)
        Random.maxInt
        (Knapsack.emptyCache identity i)
  in
    { list = List.map orphan knapsacks
    , peak = force <| List.maximum <| List.map .peak knapsacks
    }

force : Maybe a -> a
force maybeX =
  case maybeX of
    Just x -> x
    Nothing -> Debug.crash "expected Maybe to have a value"

orphan : Knapsack Int -> Priced Int
orphan knapsack =
  { state = knapsack.state
  , cost = knapsack.cost
  }

deletionChoices : DeletionCosts -> DAG -> Int -> PeakedList (Priced Int)
deletionChoices deletionCosts dag i =
  PeakedList.concatMap
    i
    (deletionChoicesHelper deletionCosts dag "") <|
    DAG.get i dag

deletionChoicesHelper :
  DeletionCosts -> DAG -> String -> Edge -> PeakedList (Priced Int)
deletionChoicesHelper deletionCosts dag key edge =
  let
    newKey = key ++ String.fromChar edge.phoneme
  in let
    rest =
      if CompletionDict.startWith newKey deletionCosts then
        PeakedList.concatMap
          edge.dst
          (deletionChoicesHelper deletionCosts dag newKey) <|
          DAG.get edge.dst dag
      else PeakedList.empty
  in
    case CompletionDict.get newKey deletionCosts of
      Nothing -> rest
      Just cost -> PeakedList.cons { state = edge.dst, cost = cost } rest
