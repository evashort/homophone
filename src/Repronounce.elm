module Repronounce exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import List
import Random
import String

import BoundaryState exposing (BoundaryState)
import CBool exposing (CBool)
import CompletionDict exposing (CompletionDict)
import DAG exposing (DAG)
import DeletionCosts exposing (DeletionCosts)
import Deletions exposing (DeletionChoice)
import Knapsack exposing (Priced, Knapsack)
import PeakedList exposing (PeakedList)
import Space exposing (Space)
import SubCosts exposing (SubCosts)
import Subs exposing (SubChoice)
import WordCosts exposing (WordCosts)

adultWordLen : Int  -- once a word is shorter than adultWordLen, it's
adultWordLen = 5    -- considered a "kid" and its cost stops decreasing in
                    -- proportion to its length. this prevents uncommon words
                    -- from slipping through the cracks because they're too
                    -- short for the cost to be significant

type alias Cache =
  { dCosts : DeletionCosts
  , sCosts : SubCosts
  , wCosts : WordCosts
  , knapsacks :
      Dict (Int, String, Int, List Space, CBool, CBool) (Knapsack State)
  , wordLists : List (List String)
  , dag : DAG
  , newWordLists : List (List String)
  }

init : DeletionCosts -> SubCosts -> WordCosts -> Cache
init dCosts sCosts wCosts =
  fst <|
    update -- to start off as done, we need one interation to get the
      2 <| -- i=0 state, and another to determine that it has no successors
      { dCosts = dCosts
      , sCosts = sCosts
      , wCosts = wCosts
      , knapsacks =
          Knapsack.emptyCache
            stateKey
            { word = Nothing
            , i = -1
            , leftovers = ""
            , kLen = 0
            , spaces = Nothing
            , startSpace = False
            }
      , wordLists = []
      , dag = DAG.fromPathLists []
      , newWordLists = []
      }

type alias State =
  { word : Maybe String
  , i : Int
  , leftovers : String
  , kLen : Int
  , spaces : Maybe (List Space)
  , startSpace : Bool
  }

setGoal : List (List String) -> Cache -> Cache
setGoal wordLists cache = { cache | newWordLists = wordLists }

update : Int -> Cache -> (Cache, Int)
update iterations cache =
  let
    dag = DAG.fromPathLists cache.newWordLists
    reusedWords =
      firstTrue <| List.map2 (/=) cache.newWordLists cache.wordLists
  in let
    cutoff = force <| DAG.getSpace reusedWords dag
    newWords = List.length cache.newWordLists - reusedWords
  in let
    reusedKnapsacks =
      Dict.filter (curry <| (>=) cutoff << fst6 << fst) cache.knapsacks
    seaLevel = if newWords > 0 then cutoff else Random.maxInt
  in let
    (knapsacks, remainingIterations) =
      Knapsack.getKnapsacks
        stateKey
        (getSuccessors cache.dCosts cache.sCosts cache.wCosts dag)
        iterations <|
        Knapsack.mapPeaks (growPlants seaLevel) reusedKnapsacks
  in
    ( { cache
      | knapsacks = knapsacks
      , wordLists = cache.newWordLists
      , dag = dag
      }
    , remainingIterations
    )

done : Cache -> Bool
done cache =
  cache.newWordLists == cache.wordLists && Knapsack.done cache.knapsacks

complete : Cache -> Bool
complete cache =
  cache.newWordLists == cache.wordLists &&
    Dict.member (toFinalKey <| DAG.length cache.dag - 1) cache.knapsacks

remainingPhonemes : Cache -> Int
remainingPhonemes cache = DAG.length cache.dag - 1 - fst6 (finalKey cache)

pronunciation : Cache -> List String
pronunciation cache =
  let knapsack = finalKnapsack cache in
    List.reverse <|
      List.filterMap .word <| knapsack.state :: knapsack.ancestors

cost : Cache -> Float
cost = .cost << finalKnapsack

finalKnapsack : Cache -> Knapsack State
finalKnapsack cache = force <| Dict.get (finalKey cache) cache.knapsacks

finalKey : Cache -> (Int, String, Int, List Space, CBool, CBool)
finalKey cache =
  let
    wordEndKeys =
      Array.filter
        (flip Dict.member cache.knapsacks) <|
        Array.map toFinalKey cache.dag.spaces
  in
    force <| Array.get (Array.length wordEndKeys - 1) wordEndKeys

toFinalKey : Int -> (Int, String, Int, List Space, CBool, CBool)
toFinalKey i = (i, "", 0, [], CBool.cFalse, CBool.cTrue)

fst6 : (a, b, c, d, e, f) -> a
fst6 (x, _, _, _, _, _) = x

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

growPlants : Int -> Int -> Int
growPlants seaLevel peak = if peak >= seaLevel then Random.maxInt else peak

initialDeletions : DeletionCosts -> DAG -> PeakedList (Priced State)
initialDeletions dCosts dag =
  PeakedList.map deletionToState <| Deletions.getDeletions dCosts dag 0

deletionToState : DeletionChoice -> Priced State
deletionToState deletion =
  { state =
      { word = Nothing
      , i = deletion.i
      , leftovers = ""
      , kLen = deletion.kLen
      , spaces = Nothing
      , startSpace = True
      }
  , cost = deletion.cost
  }

stateKey : State -> (Int, String, Int, List Space, CBool, CBool)
stateKey state =
  ( state.i
  , state.leftovers
  , state.kLen
  , Maybe.withDefault [] state.spaces
  , CBool.cBool <| state.spaces /= Nothing
  , CBool.cBool state.startSpace
  )

getSuccessors :
  DeletionCosts -> SubCosts -> WordCosts -> DAG ->
    (Int, String, Int, List Space, CBool, CBool) -> PeakedList (Priced State)
getSuccessors
  dCosts sCosts wCosts dag
    (i, leftovers, kLen, ghostlySpaces, cHasKey, cStartSpace) =
  if i == -1 then initialDeletions dCosts dag
  else
    let
      spaces = if CBool.toBool cHasKey then Just ghostlySpaces else Nothing
      startSpace = CBool.toBool cStartSpace
      bState = BoundaryState.initial
    in let
      newBState = BoundaryState.update (String.length leftovers) spaces bState
    in let
      rest =
        if CompletionDict.startWith leftovers wCosts then
          let
            peakedSubChoices =
              Subs.getSubChoices dCosts sCosts dag startSpace i
          in
            PeakedList.concatMap
              peakedSubChoices.peak
              ( getWordChoices
                  dCosts sCosts wCosts dag leftovers newBState kLen i 0.0
              )
              peakedSubChoices.list
        else PeakedList.empty
    in
      PeakedList.append
        ( List.filterMap
          ( toWordChoice
              wCosts dag "" leftovers kLen spaces startSpace bState 0 i 0.0
          )
          [1..String.length leftovers]
        )
        rest

getWordChoices :
  DeletionCosts -> SubCosts -> WordCosts -> DAG -> String -> BoundaryState ->
    Int -> Int -> Float -> SubChoice -> PeakedList (Priced State)
getWordChoices dCosts sCosts wCosts dag word bState tKLen i cost subChoice =
  let
    value = subChoice.value
    kLen = subChoice.kLen
    spaces = subChoice.spaces
    startSpace = subChoice.startSpace
  in let
    newWord = word ++ value
    newBState = BoundaryState.update (String.length value) spaces bState
    newTKLen = tKLen + kLen
    newI = subChoice.i
    newCost = cost + subChoice.cost
  in let
    rest =
      if CompletionDict.startWith newWord wCosts then
        let
          peakedSubChoices =
            Subs.getSubChoices dCosts sCosts dag startSpace newI
        in
          PeakedList.concatMap
            peakedSubChoices.peak
            ( getWordChoices
                dCosts sCosts wCosts dag newWord newBState newTKLen newI
                  newCost
            )
            peakedSubChoices.list
      else PeakedList.empty
  in
    PeakedList.append
      ( List.filterMap
          ( toWordChoice
              wCosts dag word value kLen spaces startSpace bState tKLen newI
                newCost
          )
          [1..String.length value]
      )
      rest

toWordChoice :
  WordCosts -> DAG -> String -> String -> Int -> Maybe (List Space) -> Bool ->
    BoundaryState -> Int -> Int -> Float -> Int -> Maybe (Priced State)
toWordChoice
  wCosts dag word value kLen spaces startSpace bState tKLen i cost usedLen =
  let newWord = word ++ String.left usedLen value in
    case CompletionDict.get newWord wCosts of
      Nothing -> Nothing
      Just wordCost ->
        let
          leftovers = String.dropLeft usedLen value
          newBState = BoundaryState.update usedLen spaces bState
          usedKLen =
            round <| toFloat (usedLen * kLen) / toFloat (String.length value)
        in let
          newSpaces =
            case (spaces, leftovers) of
              (_, "") -> Nothing
              (Just ss, _) -> Just <| List.filterMap (Space.minus usedLen) ss
              (Nothing, _) -> Nothing
          boundaryCost = BoundaryState.cost (tKLen + usedKLen) newBState
          leftoverKLen = kLen - usedKLen
        in
          Just
            { state =
                { word = Just newWord
                , i = i
                , leftovers = leftovers
                , kLen = leftoverKLen
                , spaces = newSpaces
                , startSpace = startSpace
                }
            , cost =
                cost + boundaryCost +
                  wordCost * toFloat (max adultWordLen <| tKLen + usedKLen)
            }
