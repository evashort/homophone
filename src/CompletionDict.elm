module CompletionDict where

import Array exposing (Array)
import List
import String

import Bisect

type alias CompletionDict valueType =
  { keys : Array String
  , values : Array valueType
  }

fromSortedPairs : List (String, valueType) -> Maybe (CompletionDict valueType)
fromSortedPairs sortedPairs =
  let keys = List.map fst sortedPairs in
    let sorted =
      case List.tail keys of
        Nothing -> True
        Just oKeys -> List.all identity <| List.map2 (<) keys oKeys
    in
      if sorted then
        Just
          { keys = Array.fromList keys
          , values = Array.fromList <| List.map snd sortedPairs
          }
      else Nothing

empty : CompletionDict valueType
empty = { keys = Array.empty, values = Array.empty }

get : String -> CompletionDict valueType -> Maybe valueType
get key d =
  let i = Bisect.bisectLeft key d.keys in
    let found =
      Maybe.withDefault False <| Maybe.map ((==) key) <| Array.get i d.keys
    in
      if found then Array.get i d.values else Nothing

startWith : String -> CompletionDict valueType -> Bool
startWith key d =
  let i = Bisect.bisectRight key d.keys in
    Maybe.withDefault
      False <|
      Maybe.map (strictStartsWith key) <| Array.get i d.keys

strictStartsWith : String -> String -> Bool
strictStartsWith sub str =
  String.length sub < String.length str && String.startsWith sub str
