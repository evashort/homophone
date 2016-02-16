module Repronounce where

import Dict
import List
import String

import CBool exposing (CBool)
import CompletionDict exposing (CompletionDict)
import DAG exposing (DAG)
import DeletionCosts exposing (DeletionCosts)
import Deletions exposing (Hiker)
import Knapsack exposing (Priced)
import PricedValue exposing (PricedValue, ValueSplit)
import SubCosts exposing (SubCosts)
import Subs exposing (SubChoice)
import WordCosts exposing (WordCosts)

type alias CostData =
  { deletionCosts : DeletionCosts
  , subCosts : SubCosts
  , wordCosts : WordCosts
  }

type alias State =
  { spellings : List String
  , rest : Hiker
  , leftovers : PricedValue
  }

repronounce : CostData -> List (List String) -> Maybe (List String)
repronounce data wordLists =
  let
    dag = DAG.fromPathLists wordLists
  in let
    knapsacks =
      Knapsack.getKnapsacks
        stateKey
        (getSuccessors data dag) <|
        getSeed data.deletionCosts dag
  in
    Maybe.map
      (List.reverse << .spellings << .state) <|
      Dict.get
        ((DAG.length dag - 1), CBool.cTrue, PricedValue.empty)
        knapsacks

getSeed : DeletionCosts -> DAG -> List (Priced State)
getSeed deletionCosts dag =
  List.map deletionToState <| Deletions.getDeletions deletionCosts dag 0

deletionToState : Priced Hiker -> Priced State
deletionToState deletion =
  { deletion
  | state =
      { spellings = []
      , rest = deletion.state
      , leftovers = []
      }
  }

stateKey : State -> (Int, CBool, PricedValue)
stateKey state =
  (state.rest.i, CBool.cBool state.rest.inSpace, state.leftovers)

getSuccessors : CostData -> DAG -> State -> List (Priced State)
getSuccessors data dag state =
  let
    word = PricedValue.getValue state.leftovers
  in let
    rest =
      if CompletionDict.startWith word data.wordCosts then
        List.concatMap
          (getWordChoices data dag state.spellings word 0.0) <|
          Subs.getSubChoices data.deletionCosts data.subCosts dag state.rest
      else []
  in
    List.filterMap
      (toWordChoice data.wordCosts state.spellings "" 0.0 state.rest)
      (PricedValue.getValueSplits state.leftovers)
    ++ rest

getWordChoices :
  CostData -> DAG -> List String -> String -> Float -> SubChoice ->
    List (Priced State)
getWordChoices data dag spellings word cost subChoice =
  let
    newWord = word ++ PricedValue.getValue subChoice.value
    newCost = cost + subChoice.cost
  in let
    rest =
      if CompletionDict.startWith newWord data.wordCosts then
        List.concatMap
          (getWordChoices data dag spellings newWord newCost) <|
          Subs.getSubChoices
            data.deletionCosts
            data.subCosts
            dag
            subChoice.rest
      else []
  in
    List.filterMap
      (toWordChoice data.wordCosts spellings word newCost subChoice.rest)
      (PricedValue.getValueSplits subChoice.value)
    ++ rest

toWordChoice :
  WordCosts -> List String -> String -> Float -> Hiker -> ValueSplit ->
    Maybe (Priced State)
toWordChoice wordCosts spellings word cost rest split =
  case CompletionDict.get (word ++ split.used) wordCosts of
    Nothing -> Nothing
    Just (spelling, wordCost) ->
      Just
        { state =
            { spellings = spelling :: spellings
            , rest = rest
            , leftovers = split.leftovers
            }
        , cost = cost + split.cost + wordCost
        }
