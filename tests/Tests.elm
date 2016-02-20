module Tests where

import ElmTest exposing (..)
import List
import String

import CompletionDict
import PricedValue exposing (grayCost, whiteCost)
import Respell

all : Test
all =
  suite "Respell Suite"
    [ test
        "It can repeat the same rabbit in order to create the right leftovers" <|
        assertEqual
          "abca bcab cabcar" <|
          respellExample
            [["ar"]]
            [("abca", 0.0), ("bcab", 0.0), ("cabcar", 0.0)]
            [("", [("abc", 0.0)])]
            []
    ]

respellExample :
  List (List String) -> List (String, Float) ->
    List (String, List (String, Float)) -> List (String, Float) ->
    String
respellExample sentence wordCosts subCosts deletionCosts =
  let
    maybePronouncer = -- does not support more than 10 words
        CompletionDict.fromSortedPairs <|
          List.indexedMap ((,) << toString) sentence
    maybeDeletionCosts = CompletionDict.fromSortedPairs deletionCosts
    maybeSubCosts = CompletionDict.fromSortedPairs subCosts
    maybeWordCosts =
      CompletionDict.fromSortedPairs <|
        List.map2 (,) (List.map fst wordCosts) wordCosts
  in
    case
      (maybePronouncer, maybeDeletionCosts, maybeSubCosts, maybeWordCosts)
    of
      (Just pronouncer, Just deletionCosts, Just subCosts, Just wordCosts) ->
        Respell.respell
          { pronouncer = pronouncer
          , deletionCosts = deletionCosts
          , subCosts = subCosts
          , wordCosts = wordCosts
          } <|
          String.join " " <| List.indexedMap (always << toString) sentence
      _ -> "error in test setup: unsorted data"
