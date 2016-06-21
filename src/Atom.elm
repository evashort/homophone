module Atom exposing (parse)

import Char
import String

-- the first step in parsing user input is to split it into Atoms. any number
-- of consecutive letters is a single Atom. so is any number of consecutive
-- digits. all other characters are separate Atoms.

parse : String -> List String
parse s =
  case String.uncons s of
    Nothing -> []
    Just (c, afterC) ->
      let rest = parse afterC in
        case (List.head rest, List.tail rest) of
          (Just nextAtom, Just afterNext) ->
            if charsStick c <| firstChar nextAtom then
              String.cons c nextAtom :: afterNext
            else String.fromChar c :: rest
          _ -> [ String.fromChar c ]

firstChar : String -> Char
firstChar s =
  case String.uncons s of
    Nothing -> Debug.crash "expected string to be non-empty"
    Just (c, _) -> c

charsStick : Char -> Char -> Bool
charsStick c1 c2 =
  (isLetter c1 && isLetter c2) || (Char.isDigit c1 && Char.isDigit c2)

isLetter : Char -> Bool
isLetter c = Char.isLower c || Char.isUpper c
