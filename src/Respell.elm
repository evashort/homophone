module Respell where

import Char
import String

import CompletionDict
import DeletionCosts exposing (DeletionCosts)
import NumParser
import Repronounce
import SubCosts exposing (SubCosts)
import WordCosts exposing (Pronouncer, Speller, WordCosts)

type alias TextUnit = NumParser.TextUnit

type alias Cache =
  { pronouncer : Pronouncer
  , speller : Speller
  , cache : Repronounce.Cache
  , textUnits : List TextUnit
  }

init :
  Pronouncer -> Speller -> DeletionCosts -> SubCosts -> WordCosts -> Cache
init pronouncer speller dCosts sCosts wCosts =
  { pronouncer = pronouncer
  , speller = speller
  , cache = Repronounce.init dCosts sCosts wCosts
  , textUnits = []
  }

setGoal : String -> Cache -> Cache
setGoal text cache =
  let textUnits = tokensToTextUnits cache.pronouncer <| tokenize text in
    { cache
    | cache =
        Repronounce.setGoal (List.concatMap .pathLists textUnits) cache.cache
    , textUnits = textUnits
    }

goal : Cache -> String
goal cache = String.concat <| List.map .spelling cache.textUnits

update : Int -> Cache -> Cache
update maxIterations cache =
  { cache | cache = Repronounce.update maxIterations cache.cache }

done : Cache -> Bool
done cache = Repronounce.done cache.cache

complete : Cache -> Bool
complete cache = Repronounce.complete cache.cache

remainingPhonemes : Cache -> Int
remainingPhonemes cache = Repronounce.remainingPhonemes cache.cache

cost : Cache -> Float
cost cache = Repronounce.cost cache.cache

spelling : Cache -> String
spelling cache =
  String.join
    " " <|
    List.map
      (force << (flip CompletionDict.get) cache.speller) <|
      Repronounce.pronunciation cache.cache

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
      (String.concat <| List.take knownGood tokens)
      pronouncer
  then maxUnitSizeHelper (knownGood + 1) pronouncer tokens
  else knownGood
