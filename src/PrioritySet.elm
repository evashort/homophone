module PrioritySet exposing
  ( PrioritySet, empty, singleton, isEmpty, insert, findMin, deleteMin
  , fromList
  )

import List
import PairingHeap exposing (PairingHeap)

-- a priority queue but if you insert the same thing twice, it only comes out
-- once

type alias PrioritySet comparable = PairingHeap comparable ()

empty : PrioritySet comparable
empty = PairingHeap.empty

singleton : comparable -> PrioritySet comparable
singleton k = PairingHeap.insert (k, ()) PairingHeap.empty

isEmpty : PrioritySet comparable -> Bool
isEmpty = (==) PairingHeap.empty

insert : comparable -> PrioritySet comparable -> PrioritySet comparable
insert k s = PairingHeap.insert (k, ()) s

findMin : PrioritySet comparable -> Maybe comparable
findMin = Maybe.map fst << PairingHeap.findMin

deleteMin : PrioritySet comparable -> PrioritySet comparable
deleteMin s =
  case findMin s of
    Nothing -> s
    Just k ->
      let newS = PairingHeap.deleteMin s in
        if findMin newS == Just k then deleteMin newS else newS

fromList : List comparable -> PrioritySet comparable
fromList = PairingHeap.fromList << List.map parify

parify : t -> (t, ())
parify x = (x, ())
