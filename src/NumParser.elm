module NumParser exposing (..)

import Char
import List
import String

type alias TextUnit =
  { spelling : String
  , pathLists : List (List String)
  }

parse : List String -> Maybe (TextUnit, List String)
parse tokens = Maybe.oneOf [ parseInteger tokens, parseDecimal tokens ]

parseDecimal : List String -> Maybe (TextUnit, List String)
parseDecimal tokens =
  case (List.take 2 tokens, List.drop 2 tokens) of
    ([".", block], rest) ->
      if isDigits block then
        Just <|
          ( { spelling = "." ++ block
            , pathLists = ["pYnt"] :: parseRawDigits block
            }
          , rest
          )
      else Nothing
    _ -> Nothing

parseInteger : List String -> Maybe (TextUnit, List String)
parseInteger tokens =
  case getBlocks tokens of
    (blocks, spelling, rest) ->
      case (List.head blocks, List.tail blocks) of
        (Just first, Just afterFirst) ->
          let
            pathLists =
              case
                (parseMultiplier first, List.map parseTriplet afterFirst)
              of
                (Just m, [Just b1, Just b2, Just b3, Just b4]) ->
                  m ++ [["trilyun"]] ++ say b1 ["bilyun"] ++
                    say b2 ["milyun"] ++ say b3 ["Txzund", "Txzun"] ++ b4
                (Just m, [Just b1, Just b2, Just b3]) ->
                  m ++ [["bilyun"]] ++ say b1 ["milyun"] ++
                    say b2 ["Txzund", "Txzun"] ++ b3
                (Just m, [Just b1, Just b2]) ->
                  m ++ [["milyun"]] ++ say b1 ["Txzund", "Txzun"] ++ b2
                (Just m, [Just b1]) ->
                  if String.length first > 1 then
                    m ++ [["Txzund", "Txzun"]] ++ b1
                  else parseDigits <| String.concat blocks
                _ -> List.concatMap parseDigits blocks
          in
            Just ({ spelling = spelling, pathLists = pathLists }, rest)
        _ -> Nothing

say : List (List String) -> List String -> List (List String)
say pathLists name =
  if List.isEmpty pathLists then [] else pathLists ++ [name]

parseTriplet : String -> Maybe (List (List String))
parseTriplet block =
  case String.toList block of
    ['0', '0', '0'] -> Just []
    ['0', '0', ones] -> Maybe.map list1 <| parseDigit ones
    ['0', tens, ones] -> parseTwoDigits tens ones
    _ -> parseMultiplier block

parseMultiplier : String -> Maybe (List (List String))
parseMultiplier block =
  case String.toList block of
    ['0'] -> Nothing
    ['0', _] -> Nothing
    ['0', _, _] -> Nothing
    [ones] -> Maybe.map list1 <| parseDigit ones
    [tens, ones] -> parseTwoDigits tens ones
    [hundreds, '0', '0'] -> parseHundredsDigit hundreds
    [hundreds, '0', ones] ->
      Maybe.map2 (++)
        (parseHundredsDigit hundreds) <|
        Maybe.map list1 <| parseDigit ones
    [hundreds, tens, ones] ->
      Maybe.map2
        (++)
        (parseHundredsDigit hundreds) <|
        parseTwoDigits tens ones
    _ -> Nothing

parseHundredsDigit : Char -> Maybe (List (List String))
parseHundredsDigit digit =
  case parseDigit digit of
    Just pathList ->
      Just [pathList, ["hundrud", "hundrid", "hunRd", "hundRd"]]
    Nothing -> Nothing

parseDigits : String -> List (List String)
parseDigits block =
  case String.toList block of
    ['0'] -> [["zirO", "zErO"]]
    [tens, ones] -> force <| parseTwoDigits tens ones
    ['0', _, _] -> parseRawDigits block
    [hundreds, '0', '0'] -> force <| parseHundredsDigit hundreds
    [hundreds, tens, ones] ->
      force (parseDigit hundreds) :: force (parseTwoDigits tens ones)
    ['0', _, _, _] -> parseRawDigits block
    [thousands, '0', '0', '0'] ->
      [force <| parseDigit thousands, ["Txzund", "Txzun"]]
    [thousands, '0', '0', ones] ->
      [ force <| parseDigit thousands
      , ["Txzund", "Txzun"]
      , force <| parseDigit ones
      ]
    [thousands, hundreds, '0', '0'] ->
      force (parseTwoDigits thousands hundreds) ++
        [["hundrud", "hundrid", "hunRd", "hundRd"]]
    [thousands, hundreds, tens, ones] ->
      force (parseTwoDigits thousands hundreds) ++
        force (parseTwoDigits tens ones)
    _ -> parseRawDigits block

parseRawDigits : String -> List (List String)
parseRawDigits = List.map (force << parseDigit) << String.toList

parseTwoDigits : Char -> Char -> Maybe (List (List String))
parseTwoDigits tens ones =
  case (tens, ones) of
    ('1', '0') -> Just [["ten"]]
    ('1', '1') -> Just [["ilevun", "Elevun"]]
    ('1', '2') -> Just [["twelv"]]
    ('1', '3') -> Just [["TRtEn"]]
    ('1', '4') -> Just [["fortEn"]]
    ('1', '5') -> Just [["fiftEn"]]
    ('1', '6') -> Just [["sikstEn"]]
    ('1', '7') -> Just [["sevuntEn"]]
    ('1', '8') -> Just [["AtEn"]]
    ('1', '9') -> Just [["nIntEn"]]
    ('0', '0') -> Just [["O"], ["O"]]
    (tens, '0') -> Maybe.map list1 <| parseTens tens
    (tens, ones) -> Maybe.map2 list2 (parseTens tens) (parseDigit ones)

list1 : a -> List a
list1 x = [x]

list2 : a -> a -> List a
list2 x y = [x, y]

force : Maybe a -> a
force maybeX =
  case maybeX of
    Just x -> x
    Nothing -> Debug.crash "expected Maybe to have a value"

getBlocks : List String -> (List String, String, List String)
getBlocks tokens =
  case (List.head tokens, List.tail tokens) of
    (Just block, Just afterBlock) ->
      if isDigits block then
        case getBlocksHelper afterBlock of
          (blocks, spelling, rest) ->
            (block :: blocks, block ++ spelling, rest)
      else ([], "", tokens)
    _ -> ([], "", tokens)

getBlocksHelper : List String -> (List String, String, List String)
getBlocksHelper tokens =
  case (List.take 2 tokens, List.drop 2 tokens) of
    ([",", block], afterBlock) ->
      if isDigits block then
        case getBlocksHelper afterBlock of
          (blocks, spelling, rest) ->
            (block :: blocks, "," ++ block ++ spelling, rest)
      else ([], "", tokens)
    _ -> ([], "", tokens)

isDigits : String -> Bool
isDigits token =
  case String.uncons token of
    Just (c, _) -> Char.isDigit c
    Nothing -> False

parseDigit : Char -> Maybe (List String)
parseDigit digit =
  case digit of
    '0' -> Just ["O"]
    '1' -> Just ["wun", "hwun"]
    '2' -> Just ["tW"]
    '3' -> Just ["TrE"]
    '4' -> Just ["for"]
    '5' -> Just ["fIv"]
    '6' -> Just ["siks"]
    '7' -> Just ["sevun"]
    '8' -> Just ["At"]
    '9' -> Just ["nIn"]
    _ -> Nothing

parseTens : Char -> Maybe (List String)
parseTens digit =
  case digit of
    '0' -> Just ["O"]
    '2' -> Just ["twentE", "twenE"]
    '3' -> Just ["TRdE", "TRtE"]
    '4' -> Just ["fortE"]
    '5' -> Just ["fiftE"]
    '6' -> Just ["sikstE"]
    '7' -> Just ["sevuntE", "sevunE"]
    '8' -> Just ["AtE"]
    '9' -> Just ["nIntE"]
    _ -> Nothing
