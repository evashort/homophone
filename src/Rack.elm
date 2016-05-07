module Rack where

import Html exposing (Html)
import String

import DeletionCosts exposing (DeletionCosts)
import Respell exposing (Cache)
import SubCosts exposing (SubCosts)
import WordCosts exposing (Pronouncer, Speller, WordCosts)

type alias Rack =
  { prototype : Cache
  , shelves : List Cache
  }

init : Pronouncer -> Speller -> DeletionCosts -> SubCosts -> WordCosts -> Rack
init pronouncer speller dCosts sCosts wCosts =
  let prototype = Respell.init pronouncer speller dCosts sCosts wCosts in
    { prototype = prototype, shelves = [ prototype ] }

setGoal : String -> Rack -> Rack
setGoal text rack =
  { rack
  | shelves =
      let
        (topShelves, _, laterShelves, laterLines) =
          splitOnTrue2
            ((/=) << Respell.goal)
            rack.shelves <|
            String.lines text
      in let
        (bottomShelves, _, midShelves, midLines) =
          reverse4 <|
            splitOnTrue2
              ((/=) << Respell.goal)
              (List.reverse laterShelves) <|
              List.reverse laterLines
      in let
        newMidShelves =
          case
            (List.head midShelves, List.head midLines, List.tail midLines)
          of
            (Just changedShelf, Just changedLine, Just newLines) ->
              Respell.setGoal changedLine changedShelf ::
                List.map (flip Respell.setGoal rack.prototype) newLines
            _ -> List.map (flip Respell.setGoal rack.prototype) midLines
      in
        topShelves ++ newMidShelves ++ bottomShelves
  }

splitOnTrue2 :
  (a -> b -> Bool) -> List a -> List b -> (List a, List b, List a, List b)
splitOnTrue2 predicate l1 l2 = splitOnTrue2Helper predicate [] [] l1 l2

splitOnTrue2Helper :
  (a -> b -> Bool) -> List a -> List b -> List a -> List b ->
    (List a, List b, List a, List b)
splitOnTrue2Helper predicate s1 s2 l1 l2 =
  case (List.head l1, List.tail l1, List.head l2, List.tail l2) of
    (Just x1, Just r1, Just x2, Just r2) ->
      if predicate x1 x2 then (List.reverse s1, List.reverse s2, l1, l2)
      else splitOnTrue2Helper predicate (x1 :: s1) (x2 :: s2) r1 r2
    _ -> (List.reverse s1, List.reverse s2, l1, l2)

reverse4 :
  (List a, List b, List c, List d) -> (List a, List b, List c, List d)
reverse4 (l1, l2, l3, l4) =
  (List.reverse l1, List.reverse l2, List.reverse l3, List.reverse l4)

goal : Rack -> String
goal = String.join "\n" << List.map Respell.goal << .shelves

update : Int -> Rack -> (Rack, Int)
update iterations rack =
  let
    (newShelves, remainingIterations) =
      List.foldl updateShelf ([], iterations) rack.shelves
  in
    ({ rack | shelves = List.reverse newShelves }, remainingIterations)

updateShelf : Cache -> (List Cache, Int) -> (List Cache, Int)
updateShelf shelf (newShelves, iterations) =
  let (newShelf, remainingIterations) = Respell.update iterations shelf in
    (newShelf :: newShelves, remainingIterations)

done : Rack -> Bool
done = List.foldl (&&) True << List.map Respell.done << .shelves

complete : Rack -> Bool
complete = List.foldl (&&) True << List.map Respell.complete << .shelves

costs : Rack -> List Float
costs = List.map Respell.cost << .shelves

spelling : Rack -> String
spelling = String.concat << List.map Respell.spelling << .shelves

view : Rack -> List Html
view = List.concatMap Respell.view << .shelves
