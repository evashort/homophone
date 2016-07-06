module NumParser exposing (Molecule, parse)

import Char
import List
import String

type alias Molecule =
  { spelling : String
  , pathLists : List (List String)
  }

parse : List String -> Maybe (Molecule, List String)
parse atoms = Maybe.oneOf [ parseInteger atoms, parseDecimal atoms ]

parseDecimal : List String -> Maybe (Molecule, List String)
parseDecimal atoms =
  case (List.take 2 atoms, List.drop 2 atoms) of
    ([".", block], rest) ->
      if isDigits block then
        Just <|
          ( { spelling = "." ++ block
            , pathLists = ["pcynt"] :: parseRawDigits block
            }
          , rest
          )
      else Nothing
    _ -> Nothing

parseInteger : List String -> Maybe (Molecule, List String)
parseInteger atoms =
  case getBlocks atoms of
    (blocks, spelling, rest) ->
      case (List.head blocks, List.tail blocks) of
        (Just first, Just afterFirst) ->
          let
            pathLists =
              case
                (parseMultiplier first, List.map parseTriplet afterFirst)
              of
                (Just m, [Just b1, Just b2, Just b3, Just b4]) ->
                  m ++ [["trily'n"]] ++ say b1 ["bily'n"] ++
                    say b2 ["mily'n"] ++ say b3 ["Tawz'n", "Tawz'nd"] ++ b4
                (Just m, [Just b1, Just b2, Just b3]) ->
                  m ++ [["bily'n"]] ++ say b1 ["mily'n"] ++
                    say b2 ["Tawz'n", "Tawz'nd"] ++ b3
                (Just m, [Just b1, Just b2]) ->
                  m ++ [["mily'n"]] ++ say b1 ["Tawz'n", "Tawz'nd"] ++ b2
                (Just m, [Just b1]) ->
                  if String.length first > 1 then
                    m ++ [["Tawz'n", "Tawz'nd"]] ++ b1
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
    Just pathList -> Just [pathList, ["hun'rd", "hund'rd", "hundr'd"]]
    Nothing -> Nothing

parseDigits : String -> List (List String)
parseDigits block =
  case String.toList block of
    ['0'] -> [["zYyr'w"]]
    [tens, ones] -> force <| parseTwoDigits tens ones
    ['0', _, _] -> parseRawDigits block
    [hundreds, '0', '0'] -> force <| parseHundredsDigit hundreds
    [hundreds, tens, ones] ->
      force (parseDigit hundreds) :: force (parseTwoDigits tens ones)
    ['0', _, _, _] -> parseRawDigits block
    [thousands, '0', '0', '0'] ->
      [force <| parseDigit thousands, ["Tawz'n", "Tawz'nd"]]
    [thousands, '0', '0', ones] ->
      [ force <| parseDigit thousands
      , ["Tawz'n", "Tawz'nd"]
      , force <| parseDigit ones
      ]
    [thousands, hundreds, '0', '0'] ->
      force (parseTwoDigits thousands hundreds) ++
        [["hun'rd", "hund'rd", "hundr'd"]]
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
    ('1', '1') -> Just [["'lev'n", "Yyl'v'n"]]
    ('1', '2') -> Just [["twelv"]]
    ('1', '3') -> Just [["TRrtYyn"]]
    ('1', '4') -> Just [["fcrtYyn"]]
    ('1', '5') -> Just [["f'ftYyn", "fiftYyn"]]
    ('1', '6') -> Just [["s'kstYyn", "sikstYyn"]]
    ('1', '7') -> Just [["sev'ntYyn"]]
    ('1', '8') -> Just [["'ytYyn", "eytYyn"]]
    ('1', '9') -> Just [["noyntYyn"]]
    ('0', '0') -> Just [["uw"], ["uw"]]
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
getBlocks atoms =
  case (List.head atoms, List.tail atoms) of
    (Just block, Just afterBlock) ->
      if isDigits block then
        case getBlocksHelper afterBlock of
          (blocks, spelling, rest) ->
            (block :: blocks, block ++ spelling, rest)
      else ([], "", atoms)
    _ -> ([], "", atoms)

getBlocksHelper : List String -> (List String, String, List String)
getBlocksHelper atoms =
  case (List.take 2 atoms, List.drop 2 atoms) of
    ([",", block], afterBlock) ->
      if isDigits block then
        case getBlocksHelper afterBlock of
          (blocks, spelling, rest) ->
            (block :: blocks, "," ++ block ++ spelling, rest)
      else ([], "", atoms)
    _ -> ([], "", atoms)

isDigits : String -> Bool
isDigits token =
  case String.uncons token of
    Just (c, _) -> Char.isDigit c
    Nothing -> False

parseDigit : Char -> Maybe (List String)
parseDigit digit =
  case digit of
    '0' -> Just ["uw"]
    '1' -> Just ["hwun", "wun"]
    '2' -> Just ["tWw"]
    '3' -> Just ["TrYy"]
    '4' -> Just ["fcr"]
    '5' -> Just ["foyv"]
    '6' -> Just ["siks"]
    '7' -> Just ["sev'n"]
    '8' -> Just ["eyt"]
    '9' -> Just ["noyn"]
    _ -> Nothing

parseTens : Char -> Maybe (List String)
parseTens digit =
  case digit of
    '0' -> Just ["uw"]
    '2' -> Just ["twentYy"]
    '3' -> Just ["TRrdYy"]
    '4' -> Just ["fcrtYy"]
    '5' -> Just ["fiftYy"]
    '6' -> Just ["sikstYy"]
    '7' -> Just ["sev'ntYy"]
    '8' -> Just ["eytYy"]
    '9' -> Just ["noyntYy"]
    _ -> Nothing
