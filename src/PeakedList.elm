module PeakedList exposing (..)

import Random

type alias PeakedList a =
  { list : List a
  , peak : Int
  }

empty : PeakedList a
empty = { list = [], peak = Random.minInt }

raise : Int -> PeakedList a -> PeakedList a
raise p l = { l | peak = max l.peak p }

cons : a -> PeakedList a -> PeakedList a
cons x l = { l | list = x :: l.list }

append : List a -> PeakedList a -> PeakedList a
append l r = { r | list = l ++ r.list }

concat : Int -> List (PeakedList a) -> PeakedList a
concat p ls =
  { list = List.concat <| List.map .list ls
  , peak = max p <| Maybe.withDefault p <| List.maximum <| List.map .peak ls
  }

map : (a -> b) -> PeakedList a -> PeakedList b
map f l = { l | list = List.map f l.list }

concatMap : Int -> (a -> PeakedList b) -> List a -> PeakedList b
concatMap p f l = concat p <| List.map f l
