module Respell where

import Char
import String

import CompletionDict
import DeletionCosts exposing (DeletionCosts)
import NumParser
import Repronounce exposing (CostData)
import SubCosts exposing (SubCosts)
import WordCosts exposing (Pronouncer, WordCosts)

type alias LoadedData =
  { pronouncer : Pronouncer
  , deletionCosts : DeletionCosts
  , subCosts : SubCosts
  , wordCosts : WordCosts
  }

type alias Cache = Repronounce.Cache

emptyCache : Cache
emptyCache = Repronounce.emptyCache

type alias Result = Repronounce.Result

type alias TextUnit = NumParser.TextUnit

respell : LoadedData -> Cache -> List TextUnit -> Int -> Result
respell data cache textUnits maxIterations =
  Repronounce.repronounce
    (getCostData data)
    cache
    (List.concatMap .pathLists textUnits)
    maxIterations

getTextUnits : LoadedData -> String -> List TextUnit
getTextUnits data text =
  tokensToTextUnits data.pronouncer <| tokenize text

getCostData : LoadedData -> CostData
getCostData data =
  { deletionCosts = data.deletionCosts
  , subCosts = data.subCosts
  , wordCosts = data.wordCosts
  }

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
