module Repronounce where

import Dict exposing (Dict)
import List
import Random
import String

import BoundaryState exposing (BoundaryState)
import CBool exposing (CBool)
import CompletionDict exposing (CompletionDict)
import DAG exposing (DAG)
import DeletionCosts exposing (DeletionCosts)
import Deletions
import Knapsack exposing (Priced, Knapsack)
import PeakedList exposing (PeakedList)
import Space exposing (Space)
import SubCosts exposing (SubCosts)
import Subs exposing (SubChoice)
import WordCosts exposing (WordCosts)

type alias CostData =
  { deletionCosts : DeletionCosts
  , subCosts : SubCosts
  , wordCosts : WordCosts
  }

type alias Cache =
  { knapsacks : Dict (Int, String, List Space, CBool, CBool) (Knapsack State)
  , wordLists : List (List String)
  }

emptyCache : Cache
emptyCache =
  { knapsacks =
      Knapsack.emptyCache
        stateKey
        { spelling = Nothing
        , i = -1
        , leftovers = ""
        , spaces = Nothing
        , startSpace = False
        }
  , wordLists = []
  }

type Respelling
  = InProgress
  | Done (String, Float)
  | NoSolution

type alias Result =
  { respelling : Respelling
  , cache : Cache
  }

type alias State =
  { spelling : Maybe String
  , i : Int
  , leftovers : String
  , spaces : Maybe (List Space)
  , startSpace : Bool
  }

repronounce : CostData -> Cache -> List (List String) -> Int -> Result
repronounce data cache wordLists maxIterations =
  let
    dag = DAG.fromPathLists wordLists
    reusedWords = firstTrue <| List.map2 (/=) wordLists cache.wordLists
  in let
    cutoff = force <| DAG.getSpace reusedWords dag
    newWords = List.length wordLists - reusedWords
  in let
    reusedCache =
      Dict.filter (curry <| (>=) cutoff << fst5 << fst) cache.knapsacks
    seaLevel = if newWords > 0 then cutoff else Random.maxInt
  in let
    knapsacksAndDone =
      Knapsack.getKnapsacks
        stateKey
        (getSuccessors data dag)
        (Knapsack.mapPeaks (growPlants seaLevel) reusedCache)
        maxIterations
  in
    { respelling =
        if snd knapsacksAndDone then
          case
            Dict.get
              (DAG.length dag - 1, "", [], CBool.cFalse, CBool.cTrue) <|
              fst knapsacksAndDone
          of
            Nothing -> NoSolution
            Just knapsack ->
              Done
                ( String.join
                    " " <|
                    List.reverse <|
                      List.filterMap
                        .spelling <|
                        knapsack.state :: knapsack.ancestors
                , knapsack.cost
                )
        else InProgress
    , cache = { knapsacks = fst knapsacksAndDone, wordLists = wordLists }
    }

growPlants : Int -> Int -> Int
growPlants seaLevel peak = if peak >= seaLevel then Random.maxInt else peak

fst5 : (a, b, c, d, e) -> a
fst5 (x, _, _, _, _) = x

firstTrue : List Bool -> Int
firstTrue a =
  Maybe.withDefault
    (List.length a) <|
    Maybe.map fst <| List.head <| List.filter snd <| List.indexedMap (,) a

force : Maybe a -> a
force maybeX =
  case maybeX of
    Just x -> x
    Nothing -> Debug.crash "expected Maybe to have a value"

initialDeletions : DeletionCosts -> DAG -> PeakedList (Priced State)
initialDeletions deletionCosts dag =
  PeakedList.map deletionToState <| Deletions.getDeletions deletionCosts dag 0

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
    PeakedList (Priced State)
getSuccessors data dag (i, leftovers, ghostlySpaces, cHasKey, cStartSpace) =
  if i == -1 then initialDeletions data.deletionCosts dag
  else
    let
      spaces = if CBool.toBool cHasKey then Just ghostlySpaces else Nothing
      startSpace = CBool.toBool cStartSpace
      bState = BoundaryState.initial
    in let
      newBState = BoundaryState.update (String.length leftovers) spaces bState
    in let
      rest =
        if CompletionDict.startWith leftovers data.wordCosts then
          let
            peakedSubChoices =
              Subs.getSubChoices
                data.deletionCosts data.subCosts dag startSpace i
          in
            PeakedList.concatMap
              peakedSubChoices.peak
              (getWordChoices data dag leftovers newBState i 0.0)
              peakedSubChoices.list
        else PeakedList.empty
    in
      PeakedList.append
        ( List.filterMap
          ( toWordChoice
              data.wordCosts dag "" leftovers spaces startSpace bState i 0.0
          )
          [1..String.length leftovers]
        )
        rest

getWordChoices :
  CostData -> DAG -> String -> BoundaryState -> Int -> Float -> SubChoice ->
    PeakedList (Priced State)
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
        let
          peakedSubChoices =
            Subs.getSubChoices
              data.deletionCosts data.subCosts dag startSpace newI
        in
          PeakedList.concatMap
            peakedSubChoices.peak
            (getWordChoices data dag newWord newBState newI newCost)
            peakedSubChoices.list
      else PeakedList.empty
  in
    PeakedList.append
      ( List.filterMap
          ( toWordChoice
              data.wordCosts dag word value spaces startSpace bState newI
                newCost
          )
          [1..String.length value]
      )
      rest

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
