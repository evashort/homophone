module BoundaryState exposing
  (sameSpaceCost, sameWordCost, BoundaryState, init, update)

import Array exposing (Array)

import Bead exposing (Bead)

-- starting your word on a word boundary of the input incurs sameSpaceCost.
-- ending your word on a word boundary of the input also incurs sameSpaceCost.
-- note that you can end your word on a boundary without starting the next
-- word on the same boundary. for example,
--   input = bus stop, output = bus top
-- if your word starts on a boundary and ends on a boundary, and there are no
-- boundaries in between, you get sameWordCost.

sameSpaceCost : Float
sameSpaceCost = 2.0

sameWordCost : Float
sameWordCost = 10.0

type alias BoundaryState = Int

init : BoundaryState
init = 0

update : Bead -> (BoundaryState, Float) -> (BoundaryState, Float)
update bead (state, cost) =
  let (newState, summand) = lookup (lookup table state) bead in
    if newState >= 0 && newState <= 11 then
      (newState, cost + summand)
    else
      Debug.crash <|
        "bad state transition: " ++ toString state ++ " -> " ++
          toString bead ++ " -> " ++ toString newState

table : Array (Array (BoundaryState, Float))
table = Array.fromList <| List.map Array.fromList
  [ [ (4, sameSpaceCost), (10, sameSpaceCost), (5, sameSpaceCost)
    , (3, sameSpaceCost), (2, sameSpaceCost), (2, sameSpaceCost), (0, 0.0)
    ]
  , [ (9, 0.0), (7, 0.0), (9, 0.0), (5, sameSpaceCost), (2, sameSpaceCost)
    , (6, 0.0), (1, 0.0)
    ]
  , [ (8, 0.0), (6, 0.0), (9, 0.0), (7, 0.0), (6, 0.0), (6, 0.0)
    , (0, sameWordCost + sameSpaceCost)
    ]
  , [ (9, 0.0), (7, 0.0), (9, 0.0), (7, 0.0), (6, 0.0), (6, 0.0)
    , (1, sameWordCost + sameSpaceCost)
    ]
  , [(4, 0.0), (-1, 0.0), (5, 0.0), (3, 0.0), (2, 0.0), (2, 0.0), (0, 0.0)]
  , [(5, 0.0), (11, 0.0), (5, 0.0), (3, 0.0), (2, 0.0), (2, 0.0), (1, 0.0)]
  , [ (8, 0.0), (6, 0.0), (9, 0.0), (7, 0.0), (6, 0.0), (6, 0.0)
    , (0, sameSpaceCost)
    ]
  , [ (9, 0.0), (7, 0.0), (9, 0.0), (7, 0.0), (6, 0.0), (6, 0.0)
    , (1, sameSpaceCost)
    ]
  , [(8, 0.0), (-1, 0.0), (9, 0.0), (7, 0.0), (6, 0.0), (6, 0.0), (0, 0.0)]
  , [(9, 0.0), (7, 0.0), (9, 0.0), (7, 0.0), (6, 0.0), (6, 0.0), (1, 0.0)]
  , [ (-1, 0.0), (10, 0.0), (-1, 0.0), (3, 0.0), (2, 0.0), (2, 0.0)
    , (0, sameWordCost + sameSpaceCost)
    ]
  , [ (-1, 0.0), (11, 0.0), (-1, 0.0), (3, 0.0), (2, 0.0), (2, 0.0)
    , (1, sameWordCost + sameSpaceCost)
    ]
  ]

lookup : Array a -> Int -> a
lookup a i =
  case Array.get i a of
    Just x -> x
    Nothing ->
      Debug.crash <| "index " ++ toString i ++ " out of bounds for " ++
        toString a
