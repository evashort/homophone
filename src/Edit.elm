module Edit exposing (Edit, getChoices)

import List
import String

import CompletionDict exposing (CompletionDict)
import DAG exposing (DAG, Edge)
import DeletionCosts exposing (DeletionCosts)
import Deletion exposing (Deletion)
import Search exposing (Priced)
import PeakedList exposing (PeakedList)
import PricedString exposing (PricedString)
import Space exposing (Space)
import SubCosts exposing (SubCosts)

type alias Edit =
  { value : String
  , kLen : Int
  , spaces : Maybe (List Space)
  , startSpace : Bool
  , i : Int
  , cost : Float
  }

getChoices :
  DeletionCosts -> SubCosts -> DAG -> Bool -> Int -> PeakedList Edit
getChoices dCosts sCosts dag startSpace i =
  let
    rest =
      PeakedList.concatMap
        i
        (getChoicesHelper dCosts sCosts dag "" [startSpace]) <|
        DAG.get i dag
  in
    PeakedList.append
      ( List.map
          (toRabbit startSpace i) <|
          Maybe.withDefault [] <| CompletionDict.get "" sCosts
      )
      rest

toRabbit : Bool -> Int -> PricedString -> Edit
toRabbit startSpace i (value, cost) =
  { value = value
  , kLen = 0
  , spaces = Nothing
  , startSpace = startSpace
  , i = i
  , cost = cost
  }

getChoicesHelper :
  DeletionCosts -> SubCosts -> DAG -> String -> List Bool -> Edge ->
    PeakedList Edit
getChoicesHelper dCosts sCosts dag key rPins edge =
  let
    newKey = key ++ String.fromChar edge.phoneme
    newRPins = DAG.isSpace edge.dst dag :: rPins
  in let
    rest =
      -- we don't have to modify startWith to account for identity subs
      -- because newKey != ""
      if CompletionDict.startWith newKey sCosts then
        PeakedList.concatMap
          edge.dst
          (getChoicesHelper dCosts sCosts dag newKey newRPins) <|
          DAG.get edge.dst dag
      else PeakedList.empty
  in
    case getValueChoices newKey sCosts of
      Nothing -> rest
      Just valueChoices ->
        let
          peakedDeletions = Deletion.getChoices dCosts dag edge.dst
          kLen = String.length newKey
        in
          PeakedList.raise
            peakedDeletions.peak <|
            PeakedList.append
              ( List.concatMap
                  (toEdits dag rPins peakedDeletions.list edge.dst kLen)
                  valueChoices
              )
              rest

getValueChoices : String -> SubCosts -> Maybe (List PricedString)
getValueChoices key sCosts =
  if String.length key == 1 then
    Just <|
      (key, 0.0) :: (Maybe.withDefault [] <| CompletionDict.get key sCosts)
  else CompletionDict.get key sCosts

toEdits :
  DAG -> List Bool -> List Deletion -> Int -> Int -> PricedString -> List Edit
toEdits dag rPins deletions kEnd kLen pricedValue =
  List.map (toEdit dag rPins pricedValue kEnd kLen) deletions

toEdit : DAG -> List Bool -> PricedString -> Int -> Int -> Deletion -> Edit
toEdit dag rPins (value, cost) kEnd kLen deletion =
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
