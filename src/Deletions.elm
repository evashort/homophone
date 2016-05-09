module Deletions where

import Dict
import Random
import String

import CompletionDict exposing (CompletionDict)
import DAG exposing (DAG, Edge)
import DeletionCosts exposing (DeletionCosts)
import Knapsack exposing (Priced, Knapsack)
import PeakedList exposing (PeakedList)

type alias DeletionChoice =
  { i : Int
  , kLen : Int
  , cost : Float
  }

type alias State =
  { i : Int
  , kLen : Int
  }

getDeletions : DeletionCosts -> DAG -> Int -> PeakedList DeletionChoice
getDeletions dCosts dag i =
  let
    knapsacks =
      Dict.values <|
        fst <|
          Knapsack.getKnapsacks
            stateKey
            (deletionChoices dCosts dag)
            Random.maxInt
            (Knapsack.emptyCache stateKey { i = i, kLen = 0 })
  in
    { list = List.map toDeletionChoice knapsacks
    , peak = force <| List.maximum <| List.map .peak knapsacks
    }

force : Maybe a -> a
force maybeX =
  case maybeX of
    Just x -> x
    Nothing -> Debug.crash "expected Maybe to have a value"

toDeletionChoice : Knapsack State -> DeletionChoice
toDeletionChoice knapsack =
  { i = knapsack.state.i
  , kLen = knapsack.state.kLen
  , cost = knapsack.cost
  }

stateKey : State -> (Int, Int)
stateKey state = (state.i, state.kLen)

deletionChoices :
  DeletionCosts -> DAG -> (Int, Int) -> PeakedList (Priced State)
deletionChoices dCosts dag (i, kLen) =
  PeakedList.concatMap
    i (deletionChoicesHelper dCosts dag kLen "") <|
    DAG.get i dag

deletionChoicesHelper :
  DeletionCosts -> DAG -> Int -> String -> Edge -> PeakedList (Priced State)
deletionChoicesHelper dCosts dag kLen key edge =
  let
    newKey = key ++ String.fromChar edge.phoneme
  in let
    rest =
      if CompletionDict.startWith newKey dCosts then
        PeakedList.concatMap
          edge.dst
          (deletionChoicesHelper dCosts dag kLen newKey) <|
          DAG.get edge.dst dag
      else PeakedList.empty
  in
    case CompletionDict.get newKey dCosts of
      Nothing -> rest
      Just cost ->
        PeakedList.cons
          { state = { i = edge.dst, kLen = kLen + String.length newKey }
          , cost = cost
          }
          rest
