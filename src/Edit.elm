module Edit exposing (Edit, getChoices)

import List
import String

import CompletionDict exposing (CompletionDict)
import DAG exposing (DAG, Edge)
import DeletionCosts exposing (DeletionCosts)
import Deletion exposing (Deletion)
import PeakedList exposing (PeakedList)
import Bead exposing (Bead)
import PricedString exposing (PricedString)
import SubCosts exposing (SubCosts)

-- an edit is either a replacement followed by zero or more deletions, or an
-- insertion, which is called a rabbit. this enforces a canonical order where
-- deletions come before rabbits between each pair of replacements. otherwise
-- the deletions and rabbits could be interleaved in an exponential number of
-- equivalent ways.

-- including deletions at the end of edits also avoids the problem of
-- retroactive same-space penalties. for example, if the input is
-- "scuba diver", then the output "scoop die for" should be penalized because
-- the space after "scoop" corresponds to the space after "scuba". we need to
-- know whether the "a" will be deleted at the time we make the "b" -> "p"
-- edit in order to penalize the first word correctly

type alias Edit =
  { i : Int
  , value : String
  , beads : List Bead
  , kLen : Int
  , cost : Float
  }

getChoices : DeletionCosts -> SubCosts -> DAG -> Int -> PeakedList Edit
getChoices dCosts sCosts dag i =
  let
    rest =
      PeakedList.concatMap
        i
        (getChoicesHelper dCosts sCosts dag "" [DAG.wordAt i dag]) <|
        DAG.get i dag
  in
    PeakedList.append
      ( List.map
          (toRabbit i) <|
          Maybe.withDefault [] <| CompletionDict.get "" sCosts
      )
      rest

toRabbit : Int -> PricedString -> Edit
toRabbit i (value, cost) =
  { i = i
  , value = value
  , beads = []
  , kLen = 0
  , cost = cost
  }

getChoicesHelper :
  DeletionCosts -> SubCosts -> DAG -> String -> List Int -> Edge ->
    PeakedList Edit
getChoicesHelper dCosts sCosts dag key rWords edge =
  let
    newKey = key ++ String.fromChar edge.phoneme
    newRWords = (DAG.wordAt edge.dst dag) :: rWords -- TODO: store this in dag
  in let
    rest =
      -- we don't have to modify startWith to account for identity subs
      -- because newKey != ""
      if CompletionDict.startWith newKey sCosts then
        PeakedList.concatMap
          edge.dst
          (getChoicesHelper dCosts sCosts dag newKey newRWords) <|
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
                  (toEdits dag rWords peakedDeletions.list edge.dst kLen)
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
  DAG -> List Int -> List Deletion -> Int -> Int -> PricedString -> List Edit
toEdits dag rWords deletions kStop kLen pricedValue =
  List.map (toEdit dag rWords pricedValue kStop kLen) deletions

toEdit : DAG -> List Int -> PricedString -> Int -> Int -> Deletion -> Edit
toEdit dag rWords (value, cost) kStop kLen deletion =
  { i = deletion.i
  , value = value
  , beads =
      Bead.fromWords
        (List.reverse <| DAG.wordAt deletion.i dag :: rWords) <|
        String.length value
  , kLen = kLen + deletion.kLen
  , cost = cost + deletion.cost
  }
