module Perforation where

import List
import String

type alias Perforation = Int

intact : Perforation
intact = 0

shallow : Perforation
shallow = 1

deep : Perforation
deep = 2

type alias PerforatedValue = List (Char, Perforation)

type alias ValueSplit =
  { used : String
  , leftovers : PerforatedValue
  , perforation : Perforation
  }

perforate : String -> List Int -> String -> PerforatedValue
perforate key spaces value =
  let
    keyLength = String.length key
    valueLength = String.length value
  in let
    tiers =
      case (List.head spaces, List.head <| List.reverse spaces) of
        (Just firstSpace, Just lastSpace) ->
          if keyLength == 0 then [ (0, deep) ]
          else
            [ (lastSpace + valueLength - keyLength, intact)
            , (valueLength - 1, deep)
            , (firstSpace - 1, shallow)
            ]
        _ -> []
  in
    List.indexedMap (perforatePhoneme tiers) <| String.toList value

perforatePhoneme : List (Int, Perforation) -> Int -> Char -> (Char, Perforation)
perforatePhoneme tiers index phoneme =
  (phoneme, perforateIndex tiers index)

perforateIndex : List (Int, Perforation) -> Int -> Perforation
perforateIndex tiers index =
  case (List.head tiers, List.tail tiers) of
    (Just (start, perforation), Just rest) ->
      if index >= start then perforation else perforateIndex rest index
    _ -> intact

emptyValue : PerforatedValue
emptyValue = []

getValue : PerforatedValue -> String
getValue perforatedValue = String.fromList <| List.map fst perforatedValue

getValueSplits : PerforatedValue -> List ValueSplit
getValueSplits perforatedValue =
  case (List.head perforatedValue, List.tail perforatedValue) of
    (Just first, Just rest) ->
      List.map3
        toValueSplit
        ( List.map
            (String.fromList << ((::) <| fst first) << List.reverse) <|
            List.scanl (::) [] <| List.map fst rest
        )
        (List.reverse <| List.scanl (::) [] <| List.reverse rest) <|
        List.map snd perforatedValue
    _ -> []

toValueSplit : String -> PerforatedValue -> Perforation -> ValueSplit
toValueSplit used leftovers perforation =
  { used = used, leftovers = leftovers, perforation = perforation }
