module WordCosts where

import List
import String

import CompletionDict exposing (CompletionDict)
import CostPair exposing (CostPair)
import Parser

costMultiplier : Float
costMultiplier = 3.0 / 25784.0

type alias Pronouncer = CompletionDict (List String)
type alias WordCosts = CompletionDict CostPair

type ParseError
  = InvalidTriplet String
  | NotSorted

parseErrorToString : ParseError -> String
parseErrorToString err =
  case err of
    InvalidTriplet t ->
      "\"" ++ t ++ "\" is not of the form \"spelling\tcost\tphonemes\""
    NotSorted -> "spellings are not in sorted order"

parse : String -> Result ParseError (Pronouncer, WordCosts)
parse fileContents =
  let triplets = parseTriplets fileContents in
    Result.map2
      (,)
      ( (Result.map toPronouncerPairs triplets)
        `Result.andThen`
        (Result.fromMaybe NotSorted << CompletionDict.fromSortedPairs)
      )
      ( (Result.map toWordCostPairs triplets)
        `Result.andThen`
        (Result.fromMaybe NotSorted << CompletionDict.fromSortedPairs)
      )

parseTriplets : String -> Result ParseError (List (String, Float, String))
parseTriplets fileContents =
  Parser.foldResults <|
    List.map parseTriplet <| Parser.nonEmptyLines fileContents

parseTriplet : String -> Result ParseError (String, Float, String)
parseTriplet tripletString =
  Result.fromMaybe
    (InvalidTriplet tripletString) <|
    case Parser.split3 "\t" tripletString of
      Nothing -> Nothing
      Just ("", costString, word) -> Nothing
      Just (spelling, costString, "") -> Nothing
      Just (spelling, costString, word) ->
        Maybe.map
          (sandwich spelling word << (*) costMultiplier) <|
          Result.toMaybe <| String.toFloat costString

sandwich : a -> c -> b -> (a, b, c)
sandwich x z y = (x, y, z)

toPronouncerPairs :
  List (String, Float, String) -> List (String, (List String))
toPronouncerPairs triplets =
  List.foldr collapseBySpelling [] <| List.map toSpellingPair triplets

toSpellingPair : (String, Float, String) -> (String, String)
toSpellingPair (spelling, cost, word) = (String.toLower spelling, word)

collapseBySpelling :
  (String, String) -> List (String, (List String)) ->
    List (String, (List String))
collapseBySpelling (spelling, word) pronouncerPairs =
  case (List.head pronouncerPairs, List.tail pronouncerPairs) of
    (Just (prevSpelling, words), Just rest) ->
      if prevSpelling == spelling then (prevSpelling, word :: words) :: rest
      else (spelling, [word]) :: pronouncerPairs
    _ -> (spelling, [word]) :: pronouncerPairs

toWordCostPairs : List (String, Float, String) -> List (String, CostPair)
toWordCostPairs triplets =
  List.foldr
    collapseByWord
    [] <|
    List.sortBy wordCostPairKey <| List.map toWordCostPair triplets

toWordCostPair : (String, Float, String) -> (String, CostPair)
toWordCostPair (spelling, cost, word) = (word, (spelling, cost))

wordCostPairKey : (String, CostPair) -> (String, Float)
wordCostPairKey (word, (spelling, cost)) = (word, cost)

collapseByWord :
  (String, CostPair) -> List (String, CostPair) -> List (String, CostPair)
collapseByWord wordCostPair wordCostPairs =
  case (List.head wordCostPairs, List.tail wordCostPairs) of
    (Just (prevWord, (prevSpelling, prevCost)), Just rest) ->
      if prevWord == fst wordCostPair then wordCostPair :: rest
      else wordCostPair :: wordCostPairs
    _ -> wordCostPair :: wordCostPairs
