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
getDeletions dCosts dag i =
  let
    knapsacks =
      Dict.values <|
        fst <|
          Knapsack.getKnapsacks
            identity
            (deletionChoices dCosts dag)
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
deletionChoices dCosts dag i =
  PeakedList.concatMap
    i (deletionChoicesHelper dCosts dag "") <|
    DAG.get i dag

deletionChoicesHelper :
  DeletionCosts -> DAG -> String -> Edge -> PeakedList (Priced Int)
deletionChoicesHelper dCosts dag key edge =
  let
    newKey = key ++ String.fromChar edge.phoneme
  in let
    rest =
      if CompletionDict.startWith newKey dCosts then
        PeakedList.concatMap
          edge.dst
          (deletionChoicesHelper dCosts dag newKey) <|
          DAG.get edge.dst dag
      else PeakedList.empty
  in
    case CompletionDict.get newKey dCosts of
      Nothing -> rest
      Just cost -> PeakedList.cons { state = edge.dst, cost = cost } rest
