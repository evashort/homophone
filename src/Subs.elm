module Subs where

import List
import String

import CompletionDict exposing (CompletionDict)
import CostPair exposing (CostPair)
import DAG exposing (DAG, Edge)
import DeletionCosts exposing (DeletionCosts)
import Deletions
import Knapsack exposing (Priced)
import Space exposing (Space)
import SubCosts exposing (SubCosts)

type alias SubChoice =
  { value : String
  , spaces : Maybe (List Space)
  , startSpace : Bool
  , i : Int
  , cost : Float
  }

getSubChoices :
  DeletionCosts -> SubCosts -> DAG -> Bool -> Int -> (List SubChoice, Int)
getSubChoices deletionCosts subCosts dag startSpace i =
  let
    rabbits =
      List.map
        (toRabbit startSpace i) <|
        Maybe.withDefault [] <| CompletionDict.get "" subCosts
  in
    List.foldl
      (subChoicesHelper deletionCosts subCosts dag "" [startSpace])
      (rabbits, 0) <|
      DAG.get i dag

toRabbit : Bool -> Int -> CostPair -> SubChoice
toRabbit startSpace i (value, cost) =
  { value = value
  , spaces = Nothing
  , startSpace = startSpace
  , i = i
  , cost = cost
  }

subChoicesHelper :
  DeletionCosts -> SubCosts -> DAG -> String -> List Bool -> Edge ->
    (List SubChoice, Int) -> (List SubChoice, Int)
subChoicesHelper
  deletionCosts subCosts dag key rPins edge (choices, roadblock) =
  let
    newKey = key ++ String.fromChar edge.phoneme
    newRPins = DAG.isSpace edge.dst dag :: rPins
  in let
    newChoicesAndRoadblock =
      case getValueChoices newKey subCosts of
        Nothing -> (choices, roadblock)
        Just valueChoices ->
          let
            deletionsAndRoadblock = Deletions.getDeletions deletionCosts dag edge.dst
          in
            ( List.concatMap
                (toSubChoices dag rPins (fst deletionsAndRoadblock) edge.dst)
                valueChoices
              ++ choices
            , max roadblock <| snd deletionsAndRoadblock
            )
  in
    -- we don't have to modify startWith to account for identity subs because
    -- newKey != ""
    if CompletionDict.startWith newKey subCosts then
      List.foldl
        (subChoicesHelper deletionCosts subCosts dag newKey newRPins)
        newChoicesAndRoadblock <|
        DAG.get edge.dst dag
    else
      (fst newChoicesAndRoadblock, max edge.dst <| snd newChoicesAndRoadblock)

getValueChoices : String -> SubCosts -> Maybe (List CostPair)
getValueChoices key subCosts =
  if String.length key == 1 then
    Just <|
      (key, 0.0) :: (Maybe.withDefault [] <| CompletionDict.get key subCosts)
  else CompletionDict.get key subCosts

toSubChoices :
  DAG -> List Bool -> List (Priced Int) -> Int -> CostPair -> List SubChoice
toSubChoices dag rPins deletions keyEnd pricedValue =
  List.map (toSubChoice dag rPins pricedValue keyEnd) deletions

toSubChoice : DAG -> List Bool -> CostPair -> Int -> Priced Int -> SubChoice
toSubChoice dag rPins (value, cost) keyEnd deletion =
  let startSpace = DAG.spaceInRange keyEnd deletion.state dag in
    { value = value
    , spaces =
        Just <|
          convertSpaces
            (List.reverse <| startSpace :: rPins) <|
            String.length value
    , startSpace = startSpace
    , i = deletion.state
    , cost = cost + deletion.cost
    }

convertSpaces : List Bool -> Int -> List Space
convertSpaces pins vLen =
  List.map
    (Space.fromKIndex (List.length pins - 1) vLen << fst) <|
    List.filter snd <| List.indexedMap (,) pins
