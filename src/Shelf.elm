module Shelf exposing
  (Shelf, init, setGoal, goal, update, done, complete, cost, viewGoal, view)

import Html exposing (Html)
import Html.Attributes as Attributes
import String

import CompletionDict
import DeletionCosts exposing (DeletionCosts)
import Homophone exposing (Homophone)
import Molecule exposing (Molecule)
import Pronouncer exposing (Pronouncer)
import SubCosts exposing (SubCosts)
import WordCosts exposing (Speller, WordCosts)

-- a Shelf represents a single line of user input and its respelling

type alias Shelf =
  { pronouncer : Pronouncer
  , speller : Speller
  , homophone : Homophone
  , molecules : List Molecule
  , goal : String
  }

init :
  Pronouncer -> Speller -> DeletionCosts -> SubCosts -> WordCosts -> Shelf
init pronouncer speller dCosts sCosts wCosts =
  { pronouncer = pronouncer
  , speller = speller
  , homophone = Homophone.init dCosts sCosts wCosts
  , molecules = []
  , goal = ""
  }

setGoal : String -> Shelf -> Shelf
setGoal text shelf =
  let molecules = Molecule.parse shelf.pronouncer text in
    { shelf
    | homophone =
        Homophone.setGoal
          (List.concatMap .pathLists molecules)
          shelf.homophone
    , molecules = molecules
    , goal = text
    }

goal : Shelf -> String
goal = .goal

update : Int -> Shelf -> (Shelf, Int)
update iterations shelf =
  let
    (newHomophone, remainingIterations) =
      Homophone.update iterations shelf.homophone
  in
   ({ shelf | homophone = newHomophone }, remainingIterations)

done : Shelf -> Bool
done shelf = Homophone.done shelf.homophone

complete : Shelf -> Bool
complete shelf = Homophone.complete shelf.homophone

cost : Shelf -> Float
cost shelf = Homophone.cost shelf.homophone

viewGoal : Shelf -> List (Html msg)
viewGoal shelf = List.map Molecule.view shelf.molecules ++ [ Html.text "\n" ]

view : Shelf -> List (Html msg)
view shelf =
  [ if done shelf && not (complete shelf) then
      Html.mark
        [ Attributes.style
            [ ("color", "darkgray")
            , ("background-color", "inherit")
            ]
        ]
        [ Html.text "No solution\n" ]
    else
      Html.text <|
        ( String.join
            " " <|
            List.map
              (force << (flip CompletionDict.get) shelf.speller) <|
              Homophone.pronunciation shelf.homophone
        ) ++
          if done shelf then "\n"
          else
            String.repeat
              (dotCount <| Homophone.remainingPhonemes shelf.homophone)
              "​."
            ++ "\n"
  ]

dotCount : Int -> Int
dotCount remainingPhonemes =
  max 3 <| round <| 2.33 * toFloat remainingPhonemes

force : Maybe a -> a
force maybeX =
  case maybeX of
    Just x -> x
    Nothing -> Debug.crash "expected Maybe to have a value"
