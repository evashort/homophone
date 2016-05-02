module Subs where

import List
import String

import CompletionDict exposing (CompletionDict)
import DAG exposing (DAG, Edge)
import DeletionCosts exposing (DeletionCosts)
import Deletions
import Knapsack exposing (Priced)
import PeakedList exposing (PeakedList)
import PricedString exposing (PricedString)
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
  DeletionCosts -> SubCosts -> DAG -> Bool -> Int -> PeakedList SubChoice
getSubChoices dCosts sCosts dag startSpace i =
  let
    rest =
      PeakedList.concatMap
        i
        (subChoicesHelper dCosts sCosts dag "" [startSpace]) <|
        DAG.get i dag
  in
    PeakedList.append
      ( List.map
          (toRabbit startSpace i) <|
          Maybe.withDefault [] <| CompletionDict.get "" sCosts
      )
      rest

toRabbit : Bool -> Int -> PricedString -> SubChoice
toRabbit startSpace i (value, cost) =
  { value = value
  , spaces = Nothing
  , startSpace = startSpace
  , i = i
  , cost = cost
  }

subChoicesHelper :
  DeletionCosts -> SubCosts -> DAG -> String -> List Bool -> Edge ->
    PeakedList SubChoice
subChoicesHelper dCosts sCosts dag key rPins edge =
  let
    newKey = key ++ String.fromChar edge.phoneme
    newRPins = DAG.isSpace edge.dst dag :: rPins
  in let
    rest =
      -- we don't have to modify startWith to account for identity subs because
      -- newKey != ""
      if CompletionDict.startWith newKey sCosts then
        PeakedList.concatMap
          edge.dst
          (subChoicesHelper dCosts sCosts dag newKey newRPins) <|
          DAG.get edge.dst dag
      else PeakedList.empty
  in
    case getValueChoices newKey sCosts of
      Nothing -> rest
      Just valueChoices ->
        let
          peakedDeletions = Deletions.getDeletions dCosts dag edge.dst
        in
          PeakedList.raise
            peakedDeletions.peak <|
            PeakedList.append
              ( List.concatMap
                  (toSubChoices dag rPins peakedDeletions.list edge.dst)
                  valueChoices
              )
              rest

getValueChoices : String -> SubCosts -> Maybe (List PricedString)
getValueChoices key sCosts =
  if String.length key == 1 then
    Just <|
      (key, 0.0) :: (Maybe.withDefault [] <| CompletionDict.get key sCosts)
  else CompletionDict.get key sCosts

toSubChoices :
  DAG -> List Bool -> List (Priced Int) -> Int -> PricedString ->
    List SubChoice
toSubChoices dag rPins deletions keyEnd pricedValue =
  List.map (toSubChoice dag rPins pricedValue keyEnd) deletions

toSubChoice :
  DAG -> List Bool -> PricedString -> Int -> Priced Int -> SubChoice
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
