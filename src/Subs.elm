module Subs where

import List
import String

import CompletionDict exposing (CompletionDict)
import CostPair exposing (CostPair)
import DAG exposing (DAG, Edge)
import DeletionCosts exposing (DeletionCosts)
import Deletions exposing (Hiker)
import Knapsack exposing (Priced)
import Perforation exposing (PerforatedValue)
import SubCosts exposing (SubCosts)

type alias SubChoice =
  { value : PerforatedValue
  , cost : Float
  , rest : Hiker
  }

getSubChoices :
  DeletionCosts -> SubCosts -> DAG -> Hiker -> List SubChoice
getSubChoices deletionCosts subCosts dag hiker =
  let
    spaces = if hiker.inSpace then [0] else []
  in let
    rabbits =
      List.concatMap
        (toSubChoices "" spaces [{ state = hiker, cost = 0.0 }]) <|
        Maybe.withDefault [] <| CompletionDict.get "" subCosts
    subs =
      List.concatMap
        (subChoicesHelper deletionCosts subCosts dag "" spaces) <|
        DAG.get hiker.i dag
  in
    rabbits ++ subs

subChoicesHelper :
  DeletionCosts -> SubCosts -> DAG -> String -> List Int -> Edge ->
    List SubChoice
subChoicesHelper deletionCosts subCosts dag key spaces edge =
  let
    newKey = key ++ String.fromChar edge.phoneme
  in let
    newSpaces =
      if DAG.isSpace edge.dst dag then (String.length newKey) :: spaces
      else spaces
  in let
    -- we don't have to modify startWith to account for identity subs because
    -- newKey != ""
    rest =
      if CompletionDict.startWith newKey subCosts then
        List.concatMap
          (subChoicesHelper deletionCosts subCosts dag newKey newSpaces) <|
          DAG.get edge.dst dag
      else []
  in
    case getValueChoices newKey subCosts of
      Nothing -> rest
      Just valueChoices ->
        let deletions = Deletions.getDeletions deletionCosts dag edge.dst in
          List.concatMap
            (toSubChoices newKey newSpaces deletions)
            valueChoices
          ++ rest

getValueChoices : String -> SubCosts -> Maybe (List CostPair)
getValueChoices key subCosts =
  if String.length key == 1 then
    Just <|
      (key, 0.0) :: (Maybe.withDefault [] <| CompletionDict.get key subCosts)
  else CompletionDict.get key subCosts

toSubChoices :
  String -> List Int -> List (Priced Hiker) -> CostPair -> List SubChoice
toSubChoices key spaces deletions value =
  List.map (toSubChoice key spaces value) deletions

toSubChoice : String -> List Int -> CostPair -> Priced Hiker -> SubChoice
toSubChoice key spaces pricedValue deletion =
  let
    deletionsAddedSpace =
      deletion.state.inSpace &&
        ( Maybe.withDefault
            True <|
            Maybe.map ((>) <| String.length key) <| List.head spaces
        )
  in let
    newSpaces =
      if deletionsAddedSpace then (String.length key) :: spaces else spaces
  in
    { value =
        Perforation.perforate key (List.reverse newSpaces) <| fst pricedValue
    , cost = (snd pricedValue) + deletion.cost
    , rest = deletion.state
    }
