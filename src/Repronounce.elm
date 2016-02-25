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
  , rspace : Bool
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
      , rspace = True
      , leftovers = ""
      }
  , cost = deletion.cost
  }

stateKey : State -> (Int, CBool, String)
stateKey state =
  (state.i, CBool.cBool state.rspace, state.leftovers)

getSuccessors : CostData -> DAG -> (Int, CBool, String) -> List (Priced State)
getSuccessors data dag (i, cRspace, leftovers) =
  let
    rspace = CBool.toBool cRspace
  in let
    rest =
      if CompletionDict.startWith leftovers data.wordCosts then
        List.concatMap
          (getWordChoices data dag leftovers i rspace 0.0) <|
          Subs.getSubChoices data.deletionCosts data.subCosts dag i
      else []
  in
    List.filterMap
      ( toWordChoice data.wordCosts dag "" i False rspace 0.0
          { value = leftovers
          , cost = 3.14159
          , i = i
          , valueEnd = i
          , key = "12"
          }
      )
      [1..String.length leftovers]
    ++ rest

getWordChoices :
  CostData -> DAG -> String -> Int -> Bool -> Float -> SubChoice ->
    List (Priced State)
getWordChoices data dag word i lspace cost subChoice =
  let
    newWord = word ++ subChoice.value
    newCost = cost + subChoice.cost
    rspace =
      if subChoice.key == "" then lspace
      else DAG.spaceInRange subChoice.valueEnd subChoice.i dag
  in let
    rest =
      if CompletionDict.startWith newWord data.wordCosts then
        List.concatMap
          (getWordChoices data dag newWord subChoice.i rspace newCost) <|
          Subs.getSubChoices data.deletionCosts data.subCosts dag subChoice.i
      else []
  in
    List.filterMap
      (toWordChoice data.wordCosts dag word i lspace rspace newCost subChoice)
      [1..String.length subChoice.value]
    ++ rest

toWordChoice :
  WordCosts -> DAG -> String -> Int -> Bool -> Bool -> Float -> SubChoice ->
    Int -> Maybe (Priced State)
toWordChoice wordCosts dag word i lspace rspace cost subChoice usedLen =
  let used = String.left usedLen subChoice.value in
    case CompletionDict.get (word ++ used) wordCosts of
      Nothing -> Nothing
      Just (spelling, wordCost) ->
        let
          leftovers =
            String.right
              (String.length subChoice.value - usedLen)
              subChoice.value
        in let
          boundaryCost =
            if subChoice.key == "" then
              if rspace then deepCost else 0.0
            else if String.length subChoice.key == 1 then
              if leftovers == "" then
                if rspace then deepCost else 0.0
              else
                if lspace || rspace then shallowCost else 0.0
            else
              if DAG.spaceInRange (i + 1) (subChoice.valueEnd - 1) dag then
                shallowCost
              else
                if leftovers == "" && rspace then deepCost else 0.0
        in
          Just
            { state =
                { spelling = Just spelling
                , i = subChoice.i
                , rspace = rspace
                , leftovers = leftovers
                }
            , cost = cost + boundaryCost + wordCost
            }
