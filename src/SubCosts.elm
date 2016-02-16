module SubCosts where

import List
import String

import CompletionDict exposing (CompletionDict)
import CostPair exposing (CostPair)
import Parser

type alias SubCosts = CompletionDict (List CostPair)

type ParseError
  = InvalidCostPair String
  | NoValues String
  | KeyEqualsValue String
  | NotSorted

parseErrorToString : ParseError -> String
parseErrorToString err =
  case err of
    InvalidCostPair p -> "\"" ++ p ++ "\" is not of the form \"value=cost\""
    NoValues k -> "no values for key \"" ++ k ++ "\""
    KeyEqualsValue k -> "key \"" ++ k ++ "\" has identical value"
    NotSorted -> "keys are not in sorted order"

parse : String -> Result ParseError SubCosts
parse fileContents =
  parseSortedMenus fileContents
  `Result.andThen`
  (Result.fromMaybe NotSorted << CompletionDict.fromSortedPairs)

parseSortedMenus : String -> Result ParseError (List (String, List CostPair))
parseSortedMenus fileContents =
  Parser.foldResults <|
    List.map parseMenu <| Parser.nonEmptyLines fileContents

parseMenu : String -> Result ParseError (String, List CostPair)
parseMenu menuString =
  let tokens = String.split " " menuString in
    case (List.head tokens, List.tail tokens) of
      (Just key, Just []) -> Err <| NoValues key
      (Just key, Just menu) ->
        (parseCostPairs menu) `Result.andThen` (addKey key)
      _ -> Err <| NoValues "this should never happen"

parseCostPairs : (List String) -> Result ParseError (List CostPair)
parseCostPairs menu =
  Parser.foldResults <| List.map parseSubCost menu

parseSubCost : String -> Result ParseError CostPair
parseSubCost costPairString =
  Result.fromMaybe
    (InvalidCostPair costPairString) <|
    CostPair.parse costPairString

addKey :
  String -> List CostPair -> Result ParseError (String, (List CostPair))
addKey key costPairs =
  if List.member key <| List.map fst costPairs then Err <| KeyEqualsValue key
  else Ok (key, costPairs)
