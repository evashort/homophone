module Respell exposing (..)

import Char
import Html exposing (Html)
import Html.Attributes as Attributes
import String

import CompletionDict
import DeletionCosts exposing (DeletionCosts)
import NumParser
import Pronouncer exposing (Pronouncer)
import Repronounce
import SubCosts exposing (SubCosts)
import WordCosts exposing (Speller, WordCosts)

type alias TextUnit = NumParser.TextUnit

type alias Cache =
  { pronouncer : Pronouncer
  , speller : Speller
  , cache : Repronounce.Cache
  , textUnits : List TextUnit
  , goal : String
  }

init :
  Pronouncer -> Speller -> DeletionCosts -> SubCosts -> WordCosts -> Cache
init pronouncer speller dCosts sCosts wCosts =
  { pronouncer = pronouncer
  , speller = speller
  , cache = Repronounce.init dCosts sCosts wCosts
  , textUnits = []
  , goal = ""
  }

setGoal : String -> Cache -> Cache
setGoal text cache =
  let textUnits = tokensToTextUnits cache.pronouncer <| tokenize text in
    { cache
    | cache =
        Repronounce.setGoal (List.concatMap .pathLists textUnits) cache.cache
    , textUnits = textUnits
    , goal = text
    }

goal : Cache -> String
goal = .goal

update : Int -> Cache -> (Cache, Int)
update iterations cache =
  let
    (newCache, remainingIterations) =
      Repronounce.update iterations cache.cache
  in
   ({ cache | cache = newCache }, remainingIterations)

done : Cache -> Bool
done cache = Repronounce.done cache.cache

complete : Cache -> Bool
complete cache = Repronounce.complete cache.cache

cost : Cache -> Float
cost cache = Repronounce.cost cache.cache

spelling : Cache -> String
spelling cache =
  ( String.join
      " " <|
      List.map
        (force << (flip CompletionDict.get) cache.speller) <|
        Repronounce.pronunciation cache.cache
  ) ++
    if done cache then "\n"
    else
      String.repeat
        (dotCount <| Repronounce.remainingPhonemes cache.cache)
        "​."
      ++ "\n"

dotCount : Int -> Int
dotCount remainingPhonemes =
  max 3 <| round <| 2.33 * toFloat remainingPhonemes

view : Cache -> List (Html msg)
view cache = List.map viewTextUnit cache.textUnits ++ [ Html.text "\n" ]

viewTextUnit : TextUnit -> Html msg
viewTextUnit textUnit =
  if List.isEmpty textUnit.pathLists &&
    isPronounced (firstChar textUnit.spelling) then
    Html.mark
      [ Attributes.style
          [ ("border-radius", "3pt")
          , ("color", "transparent")
          , ("background-color", "#ffdddd")
          ]
      ]
      [ Html.text textUnit.spelling ]
    else Html.text textUnit.spelling

isPronounced : Char -> Bool
isPronounced c = Char.isLower c || Char.isUpper c || Char.isDigit c

force : Maybe a -> a
force maybeX =
  case maybeX of
    Just x -> x
    Nothing -> Debug.crash "expected Maybe to have a value"

tokenize : String -> List String
tokenize s =
  case String.uncons s of
    Nothing -> []
    Just (c, afterC) ->
      let rest = tokenize afterC in
        case (List.head rest, List.tail rest) of
          (Just nextToken, Just afterNext) ->
            if charsStick c <| firstChar nextToken then
              String.cons c nextToken :: afterNext
            else String.fromChar c :: rest
          _ -> [ String.fromChar c ]

firstChar : String -> Char
firstChar s =
  case String.uncons s of
    Nothing -> Debug.crash "expected string to be non-empty"
    Just (c, _) -> c

charsStick : Char -> Char -> Bool
charsStick c1 c2 =
  (isLetter c1 && isLetter c2) || (Char.isDigit c1 && Char.isDigit c2)

isLetter : Char -> Bool
isLetter c = Char.isLower c || Char.isUpper c

tokensToTextUnits : Pronouncer -> List String -> List TextUnit
tokensToTextUnits pronouncer tokens =
  case NumParser.parse tokens of
    Just (unit, rest) -> unit :: tokensToTextUnits pronouncer rest
    Nothing ->
      let unitSizes = List.reverse [ 1 .. maxUnitSize pronouncer tokens ] in
        case
          Maybe.oneOf <| List.map (toTextUnit pronouncer tokens) unitSizes
        of
          Just (unit, rest) -> unit :: tokensToTextUnits pronouncer rest
          Nothing ->
            case (List.head tokens, List.tail tokens) of
              (Just first, Just rest) ->
                { spelling = first, pathLists = [] } ::
                  tokensToTextUnits pronouncer rest
              _ -> []

toTextUnit : Pronouncer -> List String -> Int -> Maybe (TextUnit, List String)
toTextUnit pronouncer tokens n =
  let spelling = String.concat <| List.take n tokens in
    case CompletionDict.get (String.toLower spelling) pronouncer of
      Nothing -> Nothing
      Just pathList ->
        Just
          ( { spelling = spelling, pathLists = [ pathList ] }
          , List.drop n tokens
          )

maxUnitSize : Pronouncer -> List String -> Int
maxUnitSize = maxUnitSizeHelper 0

maxUnitSizeHelper : Int -> Pronouncer -> List String -> Int
maxUnitSizeHelper knownGood pronouncer tokens =
  if knownGood < List.length tokens &&
    CompletionDict.startWith
      (String.toLower <| String.concat <| List.take knownGood tokens)
      pronouncer
  then maxUnitSizeHelper (knownGood + 1) pronouncer tokens
  else knownGood
