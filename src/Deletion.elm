module Deletion exposing (Deletion, getChoices)

import Dict
import Random
import String

import CompletionDict exposing (CompletionDict)
import DAG exposing (DAG, Edge)
import DeletionCosts exposing (DeletionCosts)
import Search exposing (Priced, Knapsack)
import PeakedList exposing (PeakedList)

type alias Deletion =
  { i : Int
  , kLen : Int
  , cost : Float
  }

type alias State =
  { i : Int
  , kLen : Int
  }

getChoices : DeletionCosts -> DAG -> Int -> PeakedList Deletion
getChoices dCosts dag i =
  let
    knapsacks =
      Dict.values <|
        Search.knapsacks <|
          fst <|
            Search.update
              Random.maxInt <|
              Search.singleton
                stateKey
                (getSuccessors dCosts dag)
                { i = i, kLen = 0 }
  in
    { list = List.map toDeletion knapsacks
    , peak = force <| List.maximum <| List.map .peak knapsacks
    }

force : Maybe a -> a
force maybeX =
  case maybeX of
    Just x -> x
    Nothing -> Debug.crash "expected Maybe to have a value"

toDeletion : Knapsack State -> Deletion
toDeletion knapsack =
  { i = knapsack.state.i
  , kLen = knapsack.state.kLen
  , cost = knapsack.cost
  }

stateKey : State -> (Int, Int)
stateKey state = (state.i, state.kLen)

getSuccessors :
  DeletionCosts -> DAG -> (Int, Int) -> PeakedList (Priced State)
getSuccessors dCosts dag (i, kLen) =
  PeakedList.concatMap
    i
    (getSuccessorsHelper dCosts dag kLen "") <|
    DAG.get i dag

getSuccessorsHelper :
  DeletionCosts -> DAG -> Int -> String -> Edge -> PeakedList (Priced State)
getSuccessorsHelper dCosts dag kLen key edge =
  let
    newKey = key ++ String.fromChar edge.phoneme
  in let
    rest =
      if CompletionDict.startWith newKey dCosts then
        PeakedList.concatMap
          edge.dst
          (getSuccessorsHelper dCosts dag kLen newKey) <|
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
