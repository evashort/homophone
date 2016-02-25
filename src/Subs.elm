module Subs where

import List
import String

import CompletionDict exposing (CompletionDict)
import CostPair exposing (CostPair)
import DAG exposing (DAG, Edge)
import DeletionCosts exposing (DeletionCosts)
import Deletions
import Knapsack exposing (Priced)
import SubCosts exposing (SubCosts)

type alias SubChoice =
  { value : String
  , cost : Float
  , i : Int
  , valueEnd : Int
  , pluralKey : Bool
  }

getSubChoices :
  DeletionCosts -> SubCosts -> DAG -> Int -> List SubChoice
getSubChoices deletionCosts subCosts dag i =
  let
    rabbits =
      List.concatMap
        (toSubChoices False i [{ state = i, cost = 0.0 }]) <|
        Maybe.withDefault [] <| CompletionDict.get "" subCosts
    subs =
      List.concatMap
        (subChoicesHelper deletionCosts subCosts dag "") <|
        DAG.get i dag
  in
    rabbits ++ subs

subChoicesHelper :
  DeletionCosts -> SubCosts -> DAG -> String -> Edge -> List SubChoice
subChoicesHelper deletionCosts subCosts dag key edge =
  let
    newKey = key ++ String.fromChar edge.phoneme
  in let
    -- we don't have to modify startWith to account for identity subs because
    -- newKey != ""
    rest =
      if CompletionDict.startWith newKey subCosts then
        List.concatMap
          (subChoicesHelper deletionCosts subCosts dag newKey) <|
          DAG.get edge.dst dag
      else []
  in
    case getValueChoices newKey subCosts of
      Nothing -> rest
      Just valueChoices ->
        let
          deletions = Deletions.getDeletions deletionCosts dag edge.dst
          pluralKey = String.length newKey > 1
        in
          List.concatMap
            (toSubChoices pluralKey edge.dst deletions)
            valueChoices
          ++ rest

getValueChoices : String -> SubCosts -> Maybe (List CostPair)
getValueChoices key subCosts =
  if String.length key == 1 then
    Just <|
      (key, 0.0) :: (Maybe.withDefault [] <| CompletionDict.get key subCosts)
  else CompletionDict.get key subCosts

toSubChoices : Bool -> Int -> List (Priced Int) -> CostPair -> List SubChoice
toSubChoices pluralKey valueEnd deletions pricedValue =
  List.map (toSubChoice pluralKey pricedValue valueEnd) deletions

toSubChoice : Bool -> CostPair -> Int -> Priced Int -> SubChoice
toSubChoice pluralKey pricedValue valueEnd deletion =
  { value = fst pricedValue
  , cost = snd pricedValue + deletion.cost
  , i = deletion.state
  , valueEnd = valueEnd
  , pluralKey = pluralKey
  }
