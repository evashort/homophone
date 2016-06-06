module Pronouncer exposing (..)

import List
import String

import CompletionDict exposing (CompletionDict)
import Parser

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
parse fileContents =
  Result.andThen
    (parsePairs fileContents) <|
    Result.fromMaybe NotSorted << CompletionDict.fromSortedPairs

parsePairs : String -> Result ParseError (List (String, List String))
parsePairs fileContents =
  Parser.foldResults <|
    List.map parsePair <| Parser.nonEmptyLines fileContents

parsePair : String -> Result ParseError (String, List String)
parsePair text =
  Result.fromMaybe
    (InvalidPair text) <|
    case Parser.split2 "\t" text of
      Nothing -> Nothing
      Just ("", _) -> Nothing
      Just (spelling, wordChoicesString) ->
        case String.split " " wordChoicesString of
          [] -> Nothing
          wordChoices ->
            if List.member "" wordChoices then Nothing
            else Just (spelling, wordChoices)
