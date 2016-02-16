module Respell where

import Char
import String

import CompletionDict
import DeletionCosts exposing (DeletionCosts)
import Repronounce exposing (CostData)
import SubCosts exposing (SubCosts)
import WordCosts exposing (Pronouncer, WordCosts)

type alias LoadedData =
  { pronouncer : Pronouncer
  , deletionCosts : DeletionCosts
  , subCosts : SubCosts
  , wordCosts : WordCosts
  }

respell : LoadedData -> String -> String
respell data text =
  String.join
    " " <|
    Maybe.withDefault
      [] <|
      Repronounce.repronounce
        (getCostData data) <|
        pronounce data.pronouncer <| String.toLower text

getCostData : LoadedData -> CostData
getCostData data =
  { deletionCosts = data.deletionCosts
  , subCosts = data.subCosts
  , wordCosts = data.wordCosts
  }

pronounce : Pronouncer -> String -> List (List String)
pronounce pronouncer text =
  case getWordList pronouncer "" text of
    Nothing -> []
    Just ([], rest) -> pronounce pronouncer rest
    Just (wordList, rest) -> wordList :: pronounce pronouncer rest

getWordList : Pronouncer -> String -> String -> Maybe (List String, String)
getWordList pronouncer subSpelling text =
  case getToken text of
    Nothing -> Nothing
    Just (token, rest) ->
      Just <|
        let spelling = subSpelling ++ token in
          let subResult =
            if CompletionDict.startWith spelling pronouncer then
              getWordList pronouncer spelling rest
            else Nothing
          in
            Maybe.withDefault
              ( Maybe.withDefault
                  [] <|
                  CompletionDict.get spelling pronouncer
              , rest
              ) <|
              case subResult of
                Just ([], _) -> Nothing
                _ -> subResult

getToken : String -> Maybe (String, String)
getToken s =
  case String.uncons s of
    Nothing -> Nothing
    Just (c, afterC) ->
      Just <|
        let subResult =
          if startsWithClassOf c afterC then getToken afterC else Nothing
        in
          case subResult of
            Nothing -> (String.fromChar c, afterC)
            Just (subToken, aftenToken) ->
              (String.cons c subToken, aftenToken)

type CharClass = Letter | Digit

startsWithClassOf : Char -> String -> Bool
startsWithClassOf c s =
  Maybe.withDefault
    False <|
    Maybe.map2
      (==)
      (classOfChar c)
      (String.uncons s `Maybe.andThen` (classOfChar << fst))

classOfChar : Char -> Maybe CharClass
classOfChar c =
  if Char.isLower c then Just Letter
  else if Char.isDigit c then Just Digit
  else Nothing
