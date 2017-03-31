module WordCosts exposing
  (Speller, WordCosts, ParseError, parseErrorToString, parse)

import List
import String

import CompletionDict exposing (CompletionDict)
import ParseUtils

costMultiplier : Float
costMultiplier = 1.9 / 25784.0

type alias Speller = CompletionDict String
type alias WordCosts = CompletionDict Float

type ParseError
  = InvalidTriplet String
  | NotSorted

parseErrorToString : ParseError -> String
parseErrorToString err =
  "error parsing speller: " ++
    case err of
      InvalidTriplet t ->
        "\"" ++ t ++ "\" is not of the form \"phonemes\tspelling\tcost\""
      NotSorted -> "phoneme strings are not in sorted order"

parse : String -> Result ParseError (Speller, WordCosts)
parse fileContents =
  let
    bothResult =
      parseTriplets fileContents |>
        Result.andThen
        (Result.fromMaybe NotSorted << CompletionDict.fromSortedPairs)
  in let
    spellerResult = Result.map (CompletionDict.map Tuple.first) bothResult
    wordCostsResult = Result.map (CompletionDict.map Tuple.second) bothResult
  in
    Result.map2 (,) spellerResult wordCostsResult

parseTriplets : String -> Result ParseError (List (String, (String, Float)))
parseTriplets fileContents =
  ParseUtils.foldResults <|
    List.map parseTriplet <| ParseUtils.nonEmptyLines fileContents

parseTriplet : String -> Result ParseError (String, (String, Float))
parseTriplet text =
  Result.fromMaybe
    (InvalidTriplet text) <|
    case ParseUtils.split3 "\t" text of
      Nothing -> Nothing
      Just ("", _, _) -> Nothing
      Just (_, _, "") -> Nothing
      Just (word, spelling, costString) ->
        Maybe.map
          ((,) word << (,) spelling << (*) costMultiplier) <|
          Result.toMaybe <| String.toFloat costString
