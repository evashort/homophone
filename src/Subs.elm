module Subs exposing (..)

import List
import String

import CompletionDict exposing (CompletionDict)
import DAG exposing (DAG, Edge)
import DeletionCosts exposing (DeletionCosts)
import Deletions exposing (DeletionChoice)
import Knapsack exposing (Priced)
import PeakedList exposing (PeakedList)
import PricedString exposing (PricedString)
import Space exposing (Space)
import SubCosts exposing (SubCosts)

type alias SubChoice =
  { value : String
  , kLen : Int
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
  , kLen = 0
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
          kLen = String.length newKey
        in
          PeakedList.raise
            peakedDeletions.peak <|
            PeakedList.append
              ( List.concatMap
                  (toSubChoices dag rPins peakedDeletions.list edge.dst kLen)
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
  DAG -> List Bool -> List DeletionChoice -> Int -> Int -> PricedString ->
    List SubChoice
toSubChoices dag rPins deletions kEnd kLen pricedValue =
  List.map (toSubChoice dag rPins pricedValue kEnd kLen) deletions

toSubChoice :
  DAG -> List Bool -> PricedString -> Int -> Int -> DeletionChoice ->
    SubChoice
toSubChoice dag rPins (value, cost) kEnd kLen deletion =
  let startSpace = DAG.spaceInRange kEnd deletion.i dag in
    { value = value
    , kLen = kLen + deletion.kLen
    , spaces =
        Just <|
          convertSpaces
            (List.reverse <| startSpace :: rPins) <|
            String.length value
    , startSpace = startSpace
    , i = deletion.i
    , cost = cost + deletion.cost
    }

convertSpaces : List Bool -> Int -> List Space
convertSpaces pins vLen =
  List.map
    (Space.fromKIndex (List.length pins - 1) vLen << fst) <|
    List.filter snd <| List.indexedMap (,) pins
