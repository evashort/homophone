module Molecule exposing (Molecule, view, parse)

import Char
import Html exposing (Html)
import Html.Attributes as Attributes
import String

import Atom
import CompletionDict
import NumParser
import Pronouncer exposing (Pronouncer)

-- Molecules map substrings of user input to one or more dictionary words,
-- which in turn have one or more pronunciations. Molecule boundaries are
-- chosen using a greedy strategy to maximize pronouncability, and
-- unpronouncable molecules containing letters or digits are highlighted in
-- red.

type alias Molecule = NumParser.Molecule

view : Molecule -> Html msg
view molecule =
  if List.isEmpty molecule.pathLists &&
    isPronounced (firstChar molecule.spelling) then
    Html.mark
      [ Attributes.style
          [ ("border-radius", "3pt")
          , ("color", "transparent")
          , ("background-color", "#ffdddd")
          ]
      ]
      [ Html.text molecule.spelling ]
    else Html.text molecule.spelling

isPronounced : Char -> Bool
isPronounced c = Char.isLower c || Char.isUpper c || Char.isDigit c

firstChar : String -> Char
firstChar s =
  case String.uncons s of
    Nothing -> Debug.crash "expected string to be non-empty"
    Just (c, _) -> c

parse : Pronouncer -> String -> List Molecule
parse pronouncer s = parseAtoms pronouncer <| Atom.parse s

parseAtoms : Pronouncer -> List String -> List Molecule
parseAtoms pronouncer atoms =
  case NumParser.parse atoms of
    Just (molecule, rest) -> molecule :: parseAtoms pronouncer rest
    Nothing ->
      let sizes = List.reverse [ 1 .. maxMoleculeSize pronouncer atoms ] in
        case
          Maybe.oneOf <| List.map (parseNAtoms pronouncer atoms) sizes
        of
          Just (molecule, rest) -> molecule :: parseAtoms pronouncer rest
          Nothing ->
            case (List.head atoms, List.tail atoms) of
              (Just first, Just rest) ->
                { spelling = first, pathLists = [] } ::
                  parseAtoms pronouncer rest
              _ -> []

parseNAtoms :
  Pronouncer -> List String -> Int -> Maybe (Molecule, List String)
parseNAtoms pronouncer atoms n =
  let spelling = String.concat <| List.take n atoms in
    case CompletionDict.get (String.toLower spelling) pronouncer of
      Nothing -> Nothing
      Just pathList ->
        Just
          ( { spelling = spelling, pathLists = [ pathList ] }
          , List.drop n atoms
          )

maxMoleculeSize : Pronouncer -> List String -> Int
maxMoleculeSize = maxMoleculeSizeHelper 0

maxMoleculeSizeHelper : Int -> Pronouncer -> List String -> Int
maxMoleculeSizeHelper knownGood pronouncer atoms =
  if knownGood < List.length atoms &&
    CompletionDict.startWith
      (String.toLower <| String.concat <| List.take knownGood atoms)
      pronouncer
  then maxMoleculeSizeHelper (knownGood + 1) pronouncer atoms
  else knownGood
