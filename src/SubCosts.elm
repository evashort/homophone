module SubCosts exposing (SubCosts, ParseError, parseErrorToString, parse)

import List
import String

import CompletionDict exposing (CompletionDict)
import ParseUtils
import PricedString exposing (PricedString)

type alias SubCosts = CompletionDict (List PricedString)

type ParseError
  = InvalidPricedString String
  | NoValues String
  | KeyEqualsValue String
  | NotSorted

parseErrorToString : ParseError -> String
parseErrorToString err =
  "error parsing substitutions: " ++
    case err of
      InvalidPricedString p ->
        "\"" ++ p ++ "\" is not of the form \"value=cost\""
      NoValues k -> "no values for key \"" ++ k ++ "\""
      KeyEqualsValue k -> "key \"" ++ k ++ "\" has identical value"
      NotSorted -> "keys are not in sorted order"

parse : String -> Result ParseError SubCosts
parse =
  parseMenus >>
    Result.andThen
    (Result.fromMaybe NotSorted << CompletionDict.fromSortedPairs)

parseMenus : String -> Result ParseError (List (String, List PricedString))
parseMenus =
  ParseUtils.foldResults << List.map parseMenu << ParseUtils.nonEmptyLines

parseMenu : String -> Result ParseError (String, List PricedString)
parseMenu text =
  let tokens = String.split " " text in
    case (List.head tokens, List.tail tokens) of
      (Just key, Just []) -> Err <| NoValues key
      (Just key, Just menu) ->
        parseMenuItems menu |> Result.andThen (addKey key)
      _ -> Debug.crash "non-empty line somehow has no tokens"

parseMenuItems : (List String) -> Result ParseError (List PricedString)
parseMenuItems menu = ParseUtils.foldResults <| List.map parseMenuItem menu

parseMenuItem : String -> Result ParseError PricedString
parseMenuItem text =
  Result.fromMaybe (InvalidPricedString text) <| PricedString.parse text

addKey :
  String -> List PricedString ->
    Result ParseError (String, (List PricedString))
addKey key menuItems =
  if List.member key <| List.map Tuple.first menuItems
  then Err <| KeyEqualsValue key
  else Ok (key, menuItems)
