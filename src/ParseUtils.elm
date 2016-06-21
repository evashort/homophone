module ParseUtils exposing (nonEmptyLines, foldResults, split2, split3)

import List
import String

nonEmptyLines : String -> (List String)
nonEmptyLines fileContents =
  List.filter (not << String.isEmpty) <| String.lines fileContents

foldResults : List (Result x a) -> Result x (List a)
foldResults results =
  List.foldr (Result.map2 (::)) (Ok []) results

split2 : String -> String -> Maybe (String, String)
split2 sep s =
  let l0 = String.split sep s in
    case (List.head l0, List.tail l0) of
      (Just a0, Just l1) ->
        case (List.head l1, List.tail l1) of
          (Just a1, Just []) -> Just (a0, a1)
          _ -> Nothing
      _ -> Nothing

split3 : String -> String -> Maybe (String, String, String)
split3 sep s =
  let l0 = String.split sep s in
    case (List.head l0, List.tail l0) of
      (Just a0, Just l1) ->
        case (List.head l1, List.tail l1) of
          (Just a1, Just l2) ->
            case (List.head l2, List.tail l2) of
              (Just a2, Just []) -> Just (a0, a1, a2)
              _ -> Nothing
          _ -> Nothing
      _ -> Nothing
