module Homophone exposing
  ( adultWordLen, Homophone, init, setGoal, update, done, complete
  , remainingPhonemes, pronunciation, cost
  )

import Array exposing (Array)
import Dict exposing (Dict)
import List
import Random
import String

import BoundaryState exposing (BoundaryState)
import CBool exposing (CBool)
import CompletionDict exposing (CompletionDict)
import DAG exposing (DAG)
import Deletion exposing (Deletion)
import DeletionCosts exposing (DeletionCosts)
import Edit exposing (Edit)
import PeakedList exposing (PeakedList)
import Search exposing (Priced, Knapsack, Search)
import Space exposing (Space)
import SubCosts exposing (SubCosts)
import WordCosts exposing (WordCosts)

-- a Homophone is given a goal, which is a list of words each having multiple
-- pronunciations. when its update function is called, it tries to find a new
-- set of words with similar pronunciations but different word boundaries. it
-- does this by solving a variant of the knapsack problem where filling the
-- knapsack means reaching the end of the goal, and adding an item to the
-- knapsack means performing a predefined phoneme edit with a known cost, or
-- copying a phoneme unedited. after finding the successors for a partially-
-- filled knapsack, the knapsack is assigned a "peak". the peak is the index
-- in the goal where the successor function stopped looking because the
-- phoneme(s) directly after it didn't correspond to any known edit. if the
-- goal is changed, knapsacks with peaks before the first changed phoneme (not
-- touching it) don't need their successors recalculated.

adultWordLen : Float  -- once a word is shorter than adultWordLen, it's
adultWordLen = 4.75   -- considered a "kid" and its cost stops decreasing in
                      -- proportion to its length. this prevents uncommon
                      -- words from slipping through the cracks because
                      -- they're too short for the cost to be significant

type alias StateKey = (Int, String, Int, List Space, CBool, CBool)

type Goal
  = WordListsGoal
      { wordLists : List (List String)
      , knapsacks : Dict StateKey (Knapsack State)
      }
  | DAGGoal { dag : DAG, search : Search StateKey State }

type alias Homophone =
  { dCosts : DeletionCosts
  , sCosts : SubCosts
  , wCosts : WordCosts
  , wordLists : List (List String)
  , goal : Goal
  , pronunciation : List String
  , remainingPhonemes : Int
  , cost : Float
  }

init : DeletionCosts -> SubCosts -> WordCosts -> Homophone
init dCosts sCosts wCosts =
  fst <|               -- to start off as done, we need one iteration to get
    update             -- the i=0 state, and another to determine that it has
      Random.maxInt <| -- no successors. for test cases, we may need more.
      { dCosts = dCosts
      , sCosts = sCosts
      , wCosts = wCosts
      , wordLists = []
      , goal =
          DAGGoal
            { dag = DAG.empty
            , search =
                Search.singleton
                  stateKey
                  (getSuccessors dCosts sCosts wCosts DAG.empty)
                  { word = Nothing
                  , i = -1
                  , leftovers = ""
                  , kLen = 0
                  , spaces = Nothing
                  , startSpace = False
                  }
            }
        , pronunciation = []
        , remainingPhonemes = 0
        , cost = 0.0
      }

type alias State =
  { word : Maybe String
  , i : Int
  , leftovers : String
  , kLen : Int
  , spaces : Maybe (List Space)
  , startSpace : Bool
  }

setGoal : List (List String) -> Homophone -> Homophone
setGoal wordLists homophone =
  { homophone
  | goal =
      WordListsGoal <|
        case homophone.goal of
          WordListsGoal goal -> { goal | wordLists = wordLists }
          DAGGoal goal ->
            { wordLists = wordLists
            , knapsacks = Search.knapsacks goal.search
            }
  }

update : Int -> Homophone -> (Homophone, Int)
update iterations homophone =
  let
    dag =
      case homophone.goal of
        WordListsGoal goal -> DAG.fromPathLists goal.wordLists
        DAGGoal goal -> goal.dag
  in let
    search =
      case homophone.goal of
        WordListsGoal goal ->
          let
            reusedWords =
              firstTrue <| List.map2 (/=) goal.wordLists homophone.wordLists
          in let
            seaLevel = force <| DAG.getSpace reusedWords dag
          in let
            reusedKnapsacks =
              Dict.filter
                (curry <| (>=) seaLevel << fst6 << fst)
                goal.knapsacks
          in
            Search.init
              stateKey
              ( getSuccessors
                  homophone.dCosts homophone.sCosts homophone.wCosts dag
              ) <|
              if List.length goal.wordLists > reusedWords then
                Search.mapPeaks (growPlants seaLevel) reusedKnapsacks
              else reusedKnapsacks
        DAGGoal goal -> goal.search
  in let
    (newSearch, remainingIterations) = Search.update iterations search
  in let
    wordEndKeys =
      Array.filter
        (flip Dict.member <| Search.knapsacks newSearch) <|
        Array.map toFinalKey dag.spaces
  in let
    finalKey = force <| Array.get (Array.length wordEndKeys - 1) wordEndKeys
  in let
    finalKnapsack = force <| Dict.get finalKey <| Search.knapsacks newSearch
  in
    ( { homophone
      | wordLists =
          case homophone.goal of
            WordListsGoal goal -> goal.wordLists
            DAGGoal _ -> homophone.wordLists
      , goal = DAGGoal { dag = dag, search = newSearch }
      , pronunciation =
          List.reverse <|
            List.filterMap
              .word <|
              finalKnapsack.state :: finalKnapsack.ancestors
      , remainingPhonemes = DAG.length dag - 1 - fst6 finalKey
      , cost = finalKnapsack.cost
      }
    , remainingIterations
    )

done : Homophone -> Bool
done homophone =
  case homophone.goal of
    WordListsGoal _ -> False
    DAGGoal goal -> Search.done goal.search

complete : Homophone -> Bool
complete homophone =
  case homophone.goal of
    WordListsGoal _ -> False
    DAGGoal _ -> homophone.remainingPhonemes == 0

remainingPhonemes : Homophone -> Int
remainingPhonemes = .remainingPhonemes

pronunciation : Homophone -> List String
pronunciation = .pronunciation

cost : Homophone -> Float
cost = .cost

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

fst6 : (a, b, c, d, e, f) -> a
fst6 (x, _, _, _, _, _) = x

growPlants : Int -> Int -> Int
growPlants seaLevel peak = if peak >= seaLevel then Random.maxInt else peak

toFinalKey : Int -> StateKey
toFinalKey i = (i, "", 0, [], CBool.cFalse, CBool.cTrue)

initialDeletions : DeletionCosts -> DAG -> PeakedList (Priced State)
initialDeletions dCosts dag =
  PeakedList.map deletionToState <| Deletion.getChoices dCosts dag 0

deletionToState : Deletion -> Priced State
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

stateKey : State -> StateKey
stateKey state =
  ( state.i
  , state.leftovers
  , state.kLen
  , Maybe.withDefault [] state.spaces
  , CBool.cBool <| state.spaces /= Nothing
  , CBool.cBool state.startSpace
  )

getSuccessors :
  DeletionCosts -> SubCosts -> WordCosts -> DAG -> StateKey ->
    PeakedList (Priced State)
getSuccessors
  dCosts sCosts wCosts dag
    (i, leftovers, kLen, ghostlySpaces, cHasKey, cStartSpace) =
  -- i == -1 is a special state where all successors consist of zero or more
  -- deletions. no other state can have a deletion that is not preceeded by a
  -- replacement. see comment in Edit.elm.
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
            peakedEdits = Edit.getChoices dCosts sCosts dag startSpace i
          in
            PeakedList.concatMap
              peakedEdits.peak
              ( getSuccessorsHelper
                  dCosts sCosts wCosts dag leftovers newBState kLen i 0.0
              )
              peakedEdits.list
        else PeakedList.empty
    in
      PeakedList.append
        ( List.filterMap
          ( toSuccessor
              wCosts dag "" leftovers kLen spaces startSpace bState 0 i 0.0
          )
          [1..String.length leftovers]
        )
        rest

getSuccessorsHelper :
  DeletionCosts -> SubCosts -> WordCosts -> DAG -> String -> BoundaryState ->
    Int -> Int -> Float -> Edit -> PeakedList (Priced State)
getSuccessorsHelper
  dCosts sCosts wCosts dag word bState tKLen i cost edit =
  let
    value = edit.value
    kLen = edit.kLen
    spaces = edit.spaces
    startSpace = edit.startSpace
  in let
    newWord = word ++ value
    newBState = BoundaryState.update (String.length value) spaces bState
    newTKLen = tKLen + kLen
    newI = edit.i
    newCost = cost + edit.cost
  in let
    rest =
      if CompletionDict.startWith newWord wCosts then
        let
          peakedEdits = Edit.getChoices dCosts sCosts dag startSpace newI
        in
          PeakedList.concatMap
            peakedEdits.peak
            ( getSuccessorsHelper
                dCosts sCosts wCosts dag newWord newBState newTKLen newI
                  newCost
            )
            peakedEdits.list
      else PeakedList.empty
  in
    PeakedList.append
      ( List.filterMap
          ( toSuccessor
              wCosts dag word value kLen spaces startSpace bState tKLen newI
                newCost
          )
          [1..String.length value]
      )
      rest

toSuccessor :
  WordCosts -> DAG -> String -> String -> Int -> Maybe (List Space) -> Bool ->
    BoundaryState -> Int -> Int -> Float -> Int -> Maybe (Priced State)
toSuccessor
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
                  wordCost * max adultWordLen (toFloat <| tKLen + usedKLen)
            }
