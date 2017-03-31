module Pronouncer exposing (Pronouncer, ParseError, parseErrorToString, parse)

import List
import String

import CompletionDict exposing (CompletionDict)
import ParseUtils

type alias Pronouncer = CompletionDict (List String)

type ParseError
  = InvalidPair String
  | NotSorted

parseErrorToString : ParseError -> String
parseErrorToString err =
  "error parsing pronouncer: " ++
    case err of
      InvalidPair p ->
        "\"" ++ p ++ "\" is not of the form \"spelling\tpronunciations\""
      NotSorted -> "spellings are not in sorted order"

parse : String -> Result ParseError Pronouncer
parse =
  parsePairs >>
    Result.andThen
    (Result.fromMaybe NotSorted << CompletionDict.fromSortedPairs)

parsePairs : String -> Result ParseError (List (String, List String))
parsePairs =
  ParseUtils.foldResults << List.map parsePair << ParseUtils.nonEmptyLines

parsePair : String -> Result ParseError (String, List String)
parsePair text =
  Result.fromMaybe
    (InvalidPair text) <|
    case ParseUtils.split2 "\t" text of
      Nothing -> Nothing
      Just ("", _) -> Nothing
      Just (spelling, wordChoicesString) ->
        case String.split " " wordChoicesString of
          [] -> Nothing
          wordChoices ->
            if List.member "" wordChoices then Nothing
            else Just (spelling, wordChoices)
