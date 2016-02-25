module Repronounce where

import Dict
import List
import String

import CBool exposing (CBool)
import CompletionDict exposing (CompletionDict)
import DAG exposing (DAG)
import DeletionCosts exposing (DeletionCosts)
import Deletions
import Knapsack exposing (Priced)
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
  , i : Int
  , inSpace : Bool
  , leftovers : String
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
      Dict.get (DAG.length dag - 1, CBool.cTrue, "") knapsacks
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

deletionToState : Priced Int -> Priced State
deletionToState deletion =
  { state =
      { spelling = Nothing
      , i = deletion.state
      , inSpace = True
      , leftovers = ""
      }
  , cost = deletion.cost
  }

stateKey : State -> (Int, CBool, String)
stateKey state =
  (state.i, CBool.cBool state.inSpace, state.leftovers)

getSuccessors : CostData -> DAG -> (Int, CBool, String) -> List (Priced State)
getSuccessors data dag (i, cInSpace, leftovers) =
  let
    inSpace = CBool.toBool cInSpace
  in let
    rest =
      if CompletionDict.startWith leftovers data.wordCosts then
        List.concatMap
          (getWordChoices data dag leftovers i inSpace 0.0) <|
          Subs.getSubChoices data.deletionCosts data.subCosts dag i
      else []
  in
    List.filterMap
      (toWordChoice data.wordCosts dag "" i inSpace False False 0.0)
      (getValueSplits leftovers)
    ++ rest

getWordChoices :
  CostData -> DAG -> String -> Int -> Bool -> Float -> SubChoice ->
    List (Priced State)
getWordChoices data dag word i inSpace cost subChoice =
  let
    newWord = word ++ subChoice.value
    newCost = cost + subChoice.cost
    rabbit = i == subChoice.i
  in let
    newInSpace =
      if rabbit then inSpace
      else DAG.spaceInRange subChoice.valueEnd subChoice.i dag
  in let
    spaced =
      if subChoice.pluralKey then
        DAG.spaceInRange (i + 1) (subChoice.valueEnd - 1) dag
      else (String.length subChoice.value > 1) &&  (inSpace || newInSpace)
    rest =
      if CompletionDict.startWith newWord data.wordCosts then
        List.concatMap
          (getWordChoices data dag newWord subChoice.i newInSpace newCost) <|
          Subs.getSubChoices data.deletionCosts data.subCosts dag subChoice.i
      else []
  in
    List.filterMap
      ( toWordChoice
          data.wordCosts dag word subChoice.i newInSpace spaced rabbit newCost
      )
      (getValueSplits subChoice.value)
    ++ rest

getValueSplits : String -> List (String, String)
getValueSplits value = List.map (splitValue value) [1..String.length value]

splitValue : String -> Int -> (String, String)
splitValue value i =
  (String.left i value, String.right (String.length value - i) value)

toWordChoice :
  WordCosts -> DAG -> String -> Int -> Bool -> Bool -> Bool -> Float ->
    (String, String) -> Maybe (Priced State)
toWordChoice
  wordCosts dag word i inSpace spaced rabbit cost (used, leftovers) =
  case CompletionDict.get (word ++ used) wordCosts of
    Nothing -> Nothing
    Just (spelling, wordCost) ->
      let
        boundaryCost =
          if inSpace && (leftovers == "" || rabbit) then deepCost
          else
            if spaced && (leftovers /= "" || String.length used == 1) then
              shallowCost
            else 0.0
      in
        Just
          { state =
              { spelling = Just spelling
              , i = i
              , inSpace = inSpace
              , leftovers = leftovers
              }
          , cost = cost + boundaryCost + wordCost
          }
