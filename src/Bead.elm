module Bead exposing (Bead, fromWords, finish)

import Array exposing (Array)

type alias Bead = Int

fromWords : List Int -> Int -> List Bead
fromWords wordList v =
  List.map (init (Array.fromList wordList) v) [0 .. v - 1]

finish : Bead
finish = 6

init : Array Int -> Int -> Int -> Bead
init words v i =
  let
    k = Array.length words - 1
  in let
    kStart = getKStart k v i
    nextKStart = getKStart k v (i + 1)
  in let
    firstWord = lookup words kStart
  in
    if kStart < nextKStart then
      let
        lastWord = lookup words <| getKEnd k v i
        nextWord = lookup words nextKStart
      in
        case (firstWord < lastWord, lastWord < nextWord) of
          (False, False) -> 2
          (True, False) -> 3
          (True, True) -> 4
          (False, True) -> 5
    else if firstWord < lookup words (kStart + 1) then 1 else 0

getKStart : Int -> Int -> Int -> Int
getKStart k v i =
  if i < v then
    if i > 0 then clamp 0 (k - 1) <| (2 * i + k - v) // 2 else 0
  else k

getKEnd : Int -> Int -> Int -> Int
getKEnd k v i =
  if i < v - 1 then clamp 0 (k - 1) <| (2 * i + 1 + k - v) // 2 else k - 1

lookup : Array a -> Int -> a
lookup a i =
  case Array.get i a of
    Just x -> x
    Nothing ->
      Debug.crash <|
        "index " ++ toString i ++ " out of bounds for " ++ toString a
