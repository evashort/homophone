module WordCosts where

import List
import String

import CompletionDict exposing (CompletionDict)
import Parser
import PricedString exposing (PricedString)

costMultiplier : Float
costMultiplier = 1.5 / 25784.0

type alias Pronouncer = CompletionDict (List String)
type alias Speller = CompletionDict String
type alias WordCosts = CompletionDict Float

type ParseError
  = InvalidTriplet String
  | NotSorted

parseErrorToString : ParseError -> String
parseErrorToString err =
  case err of
    InvalidTriplet t ->
      "\"" ++ t ++ "\" is not of the form \"spelling\tcost\tphonemes\""
    NotSorted -> "spellings are not in sorted order"

parse : String -> Result ParseError (Pronouncer, Speller, WordCosts)
parse fileContents =
  let
    triplets = parseTriplets fileContents
  in let
    pronouncerResult =
      Result.andThen
        (Result.map toPronouncerPairs triplets) <|
        Result.fromMaybe NotSorted << CompletionDict.fromSortedPairs
    pricedSpellingResult =
      Result.andThen
        (Result.map toPricedSpellingPairs triplets) <|
        Result.fromMaybe NotSorted << CompletionDict.fromSortedPairs
  in let
    spellerResult = Result.map (CompletionDict.map fst) pricedSpellingResult
    wordCostsResult = Result.map (CompletionDict.map snd) pricedSpellingResult
  in
    Result.map3 (,,) pronouncerResult spellerResult wordCostsResult

parseTriplets : String -> Result ParseError (List (String, Float, String))
parseTriplets fileContents =
  Parser.foldResults <|
    List.map parseTriplet <| Parser.nonEmptyLines fileContents

parseTriplet : String -> Result ParseError (String, Float, String)
parseTriplet text =
  Result.fromMaybe
    (InvalidTriplet text) <|
    case Parser.split3 "\t" text of
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
  List.foldr collapseBySpelling [] <| List.map toPronouncerPair triplets

toPronouncerPair : (String, Float, String) -> (String, String)
toPronouncerPair (spelling, cost, word) = (String.toLower spelling, word)

collapseBySpelling :
  (String, String) -> List (String, (List String)) ->
    List (String, (List String))
collapseBySpelling (spelling, word) pairs =
  case (List.head pairs, List.tail pairs) of
    (Just (prevSpelling, words), Just rest) ->
      if prevSpelling == spelling then (prevSpelling, word :: words) :: rest
      else (spelling, [word]) :: pairs
    _ -> (spelling, [word]) :: pairs

toPricedSpellingPairs :
  List (String, Float, String) -> List (String, PricedString)
toPricedSpellingPairs triplets =
  List.foldr
    collapseByWord
    [] <|
    List.sortBy
      pricedSpellingPairKey <|
      List.map toPricedSpellingPair triplets

toPricedSpellingPair : (String, Float, String) -> (String, PricedString)
toPricedSpellingPair (spelling, cost, word) = (word, (spelling, cost))

pricedSpellingPairKey : (String, PricedString) -> (String, Float)
pricedSpellingPairKey (word, (spelling, cost)) = (word, cost)

collapseByWord :
  (String, PricedString) -> List (String, PricedString) ->
    List (String, PricedString)
collapseByWord pair pairs =
  case (List.head pairs, List.tail pairs) of
    (Just (prevWord, (prevSpelling, prevCost)), Just rest) ->
      if prevWord == fst pair then pair :: rest
      else pair :: pairs
    _ -> pair :: pairs
