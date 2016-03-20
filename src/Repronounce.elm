module Repronounce where

import Dict
import List
import String

import BoundaryState exposing (BoundaryState)
import CBool exposing (CBool)
import CompletionDict exposing (CompletionDict)
import DAG exposing (DAG)
import DeletionCosts exposing (DeletionCosts)
import Deletions
import Knapsack exposing (Priced)
import Space exposing (Space)
import SubCosts exposing (SubCosts)
import Subs exposing (SubChoice)
import WordCosts exposing (WordCosts)

type alias CostData =
  { deletionCosts : DeletionCosts
  , subCosts : SubCosts
  , wordCosts : WordCosts
  }

type alias State =
  { spelling : Maybe String
  , i : Int
  , leftovers : String
  , spaces : Maybe (List Space)
  , startSpace : Bool
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
        (DAG.length dag - 1, "", [], CBool.cFalse, CBool.cTrue)
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

deletionToState : Priced Int -> Priced State
deletionToState deletion =
  { state =
      { spelling = Nothing
      , i = deletion.state
      , leftovers = ""
      , spaces = Nothing
      , startSpace = True
      }
  , cost = deletion.cost
  }

stateKey : State -> (Int, String, List Space, CBool, CBool)
stateKey state =
  ( state.i
  , state.leftovers
  , Maybe.withDefault [] state.spaces
  , CBool.cBool <| state.spaces /= Nothing
  , CBool.cBool state.startSpace
  )

getSuccessors :
  CostData -> DAG -> (Int, String, List Space, CBool, CBool) ->
    List (Priced State)
getSuccessors data dag (i, leftovers, ghostlySpaces, cHasKey, cStartSpace) =
  let
    spaces = if CBool.toBool cHasKey then Just ghostlySpaces else Nothing
    startSpace = CBool.toBool cStartSpace
    bState = BoundaryState.initial
  in let
    newBState = BoundaryState.update (String.length leftovers) spaces bState
  in let
    rest =
      if CompletionDict.startWith leftovers data.wordCosts then
        List.concatMap
          (getWordChoices data dag leftovers newBState i 0.0) <|
          Subs.getSubChoices data.deletionCosts data.subCosts dag startSpace i
      else []
  in
    List.filterMap
      ( toWordChoice
          data.wordCosts dag "" leftovers spaces startSpace bState i 0.0
      )
      [1..String.length leftovers]
    ++ rest

getWordChoices :
  CostData -> DAG -> String -> BoundaryState -> Int -> Float -> SubChoice ->
    List (Priced State)
getWordChoices data dag word bState i cost subChoice =
  let
    value = subChoice.value
    spaces = subChoice.spaces
    startSpace = subChoice.startSpace
  in let
    newWord = word ++ value
    newBState = BoundaryState.update (String.length value) spaces bState
    newI = subChoice.i
    newCost = cost + subChoice.cost
  in let
    rest =
      if CompletionDict.startWith newWord data.wordCosts then
        List.concatMap
          (getWordChoices data dag newWord newBState newI newCost) <|
          Subs.getSubChoices
            data.deletionCosts data.subCosts dag startSpace newI
      else []
  in
    List.filterMap
      ( toWordChoice
          data.wordCosts dag word value spaces startSpace bState newI newCost
      )
      [1..String.length value]
    ++ rest

toWordChoice :
  WordCosts -> DAG -> String -> String -> Maybe (List Space) -> Bool ->
    BoundaryState -> Int -> Float -> Int -> Maybe (Priced State)
toWordChoice
  wordCosts dag word value spaces startSpace bState i cost usedLen =
  case CompletionDict.get (word ++ String.left usedLen value) wordCosts of
    Nothing -> Nothing
    Just (spelling, wordCost) ->
      let
        leftovers = String.dropLeft usedLen value
        newBState = BoundaryState.update usedLen spaces bState
      in let
        newSpaces =
          case (spaces, leftovers) of
            (_, "") -> Nothing
            (Just ss, _) -> Just <| List.filterMap (Space.minus usedLen) ss
            (Nothing, _) -> Nothing
        boundaryCost = BoundaryState.cost newBState
      in
        Just
          { state =
              { spelling = Just spelling
              , i = i
              , leftovers = leftovers
              , spaces = newSpaces
              , startSpace = startSpace
              }
          , cost = cost + boundaryCost + wordCost
          }
