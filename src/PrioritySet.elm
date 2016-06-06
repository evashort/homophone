module PrioritySet exposing (..)

import List
import PairingHeap exposing (PairingHeap)
import Set exposing (Set)

type alias PrioritySet comparable =
  { heap : PairingHeap comparable ()
  , set : Set comparable
  }

empty : PrioritySet comparable
empty = { heap = PairingHeap.empty, set = Set.empty }

insert : comparable -> PrioritySet comparable -> PrioritySet comparable
insert k s =
  if Set.member k s.set then s
  else
    { heap = PairingHeap.insert (k, ()) s.heap
    , set = Set.insert k s.set
    }

findMin : PrioritySet comparable -> Maybe comparable
findMin s = Maybe.map fst <| PairingHeap.findMin s.heap

deleteMin : PrioritySet comparable -> PrioritySet comparable
deleteMin s =
  case PairingHeap.findMin s.heap of
    Nothing -> s
    Just (k, _) ->
      { heap = PairingHeap.deleteMin s.heap
      , set = Set.remove k s.set
      }

fromList : List comparable -> PrioritySet comparable
fromList xs =
  let set = Set.fromList xs in
    { heap = PairingHeap.fromList <| List.map parify <| Set.toList set
    , set = set
    }

parify : t -> (t, ())
parify x = (x, ())
