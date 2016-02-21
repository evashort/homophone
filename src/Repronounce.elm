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
import Perforation exposing (Perforation, PerforatedValue, ValueSplit)
import SubCosts exposing (SubCosts)
import Subs exposing (SubChoice)
import WordCosts exposing (WordCosts)

shallowCost : Float
shallowCost = 1.0

deepCost : Float
deepCost = 2.0

type alias CostData =
  { deletionCosts : DeletionCosts
  , subCosts : SubCosts
  , wordCosts : WordCosts
  }

type alias State =
  { spelling : Maybe String
  , rest : Hiker
  , leftovers : PerforatedValue
  }

repronounce : CostData -> List (List String) -> Maybe (String, Float)
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
    case
      Dict.get
        ((DAG.length dag - 1), CBool.cTrue, Perforation.emptyValue)
        knapsacks
    of
      Nothing -> Nothing
      Just knapsack ->
        Just
          ( String.join
              " " <|
              List.reverse <|
                List.filterMap
                  .spelling <|
                  knapsack.state :: knapsack.ancestors
          , knapsack.cost
          )

getSeed : DeletionCosts -> DAG -> List (Priced State)
getSeed deletionCosts dag =
  List.map deletionToState <| Deletions.getDeletions deletionCosts dag 0

deletionToState : Priced Hiker -> Priced State
deletionToState deletion =
  { deletion
  | state =
      { spelling = Nothing
      , rest = deletion.state
      , leftovers = []
      }
  }

stateKey : State -> (Int, CBool, PerforatedValue)
stateKey state =
  (state.rest.i, CBool.cBool state.rest.inSpace, state.leftovers)

getSuccessors :
  CostData -> DAG -> (Int, CBool, PerforatedValue) -> List (Priced State)
getSuccessors data dag (i, cInSpace, leftovers) =
  let
    word = Perforation.getValue leftovers
    hiker = { i = i, inSpace = CBool.toBool cInSpace }
  in let
    rest =
      if CompletionDict.startWith word data.wordCosts then
        List.concatMap
          (getWordChoices data dag word 0.0) <|
          Subs.getSubChoices data.deletionCosts data.subCosts dag hiker
      else []
  in
    List.filterMap
      (toWordChoice data.wordCosts "" 0.0 hiker)
      (Perforation.getValueSplits leftovers)
    ++ rest

getWordChoices :
  CostData -> DAG -> String -> Float -> SubChoice -> List (Priced State)
getWordChoices data dag word cost subChoice =
  let
    newWord = word ++ Perforation.getValue subChoice.value
    newCost = cost + subChoice.cost
  in let
    rest =
      if CompletionDict.startWith newWord data.wordCosts then
        List.concatMap
          (getWordChoices data dag newWord newCost) <|
          Subs.getSubChoices
            data.deletionCosts
            data.subCosts
            dag
            subChoice.rest
      else []
  in
    List.filterMap
      (toWordChoice data.wordCosts word newCost subChoice.rest)
      (Perforation.getValueSplits subChoice.value)
    ++ rest

toWordChoice :
  WordCosts -> String -> Float -> Hiker -> ValueSplit -> Maybe (Priced State)
toWordChoice wordCosts word cost rest split =
  case CompletionDict.get (word ++ split.used) wordCosts of
    Nothing -> Nothing
    Just (spelling, wordCost) ->
      Just
        { state =
            { spelling = Just spelling
            , rest = rest
            , leftovers = split.leftovers
            }
        , cost = cost + perforationCost split.perforation + wordCost
        }

perforationCost : Perforation -> Float
perforationCost perforation =
  if perforation == Perforation.shallow then shallowCost
  else if perforation == Perforation.deep then deepCost
  else 0.0
