module DeletionCosts where

import List

import CompletionDict exposing (CompletionDict)
import CostPair exposing (CostPair)
import Parser

type alias DeletionCosts = CompletionDict Float

type ParseError
  = InvalidCostPair String
  | NotSorted

parseErrorToString : ParseError -> String
parseErrorToString err =
  case err of
    InvalidCostPair p -> "\"" ++ p ++ "\" is not of the form \"key=cost\""
    NotSorted -> "keys are not in sorted order"

parse : String -> Result ParseError DeletionCosts
parse fileContents =
  parseSortedPairs fileContents
  `Result.andThen`
  (Result.fromMaybe NotSorted << CompletionDict.fromSortedPairs)

parseSortedPairs : String -> Result ParseError (List CostPair)
parseSortedPairs fileContents =
  Parser.foldResults <|
    List.map parseDeletionCost <| Parser.nonEmptyLines fileContents

parseDeletionCost : String -> Result ParseError CostPair
parseDeletionCost costPairString =
  Result.fromMaybe
    (InvalidCostPair costPairString) <|
    CostPair.parse costPairString
