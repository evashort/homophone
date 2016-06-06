module DeletionCosts exposing (..)

import List

import CompletionDict exposing (CompletionDict)
import Parser
import PricedString exposing (PricedString)

type alias DeletionCosts = CompletionDict Float

type ParseError
  = InvalidPricedString String
  | NotSorted

parseErrorToString : ParseError -> String
parseErrorToString err =
  "error parsing deletions: " ++
    case err of
      InvalidPricedString p ->
        "\"" ++ p ++ "\" is not of the form \"key=cost\""
      NotSorted -> "keys are not in sorted order"

parse : String -> Result ParseError DeletionCosts
parse fileContents =
  Result.andThen
    (parsePricedDeletions fileContents) <|
    Result.fromMaybe NotSorted << CompletionDict.fromSortedPairs

parsePricedDeletions : String -> Result ParseError (List PricedString)
parsePricedDeletions fileContents =
  Parser.foldResults <|
    List.map parsePricedDeletion <| Parser.nonEmptyLines fileContents

parsePricedDeletion : String -> Result ParseError PricedString
parsePricedDeletion text =
  Result.fromMaybe (InvalidPricedString text) <| PricedString.parse text
