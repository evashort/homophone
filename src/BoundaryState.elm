module BoundaryState where

import List

import Space exposing (Space)

type alias BoundaryState =
  Maybe
    { startSpace : Bool
    , midSpace : Bool
    , endSpace : Bool
    }

spaceCost : Float
spaceCost = 1.0

wordCost : Float
wordCost = 3.0

initial : BoundaryState
initial = Nothing

update : Int -> Maybe (List Space) -> BoundaryState -> BoundaryState
update vLen spaces state =
  case spaces of
    Just ss ->
      Just <|
        case state of
          Just b ->
            { b
            | midSpace = b.midSpace || b.endSpace ||
                memberSatisfies (not << Space.endsWordAt (vLen - 1)) ss
            , endSpace = memberSatisfies (Space.endsWordAt (vLen - 1)) ss
            }
          Nothing ->
            { startSpace = memberSatisfies (Space.startsWordAt 0) ss
            , midSpace =
                memberSatisfies
                  ( neither
                      (Space.startsWordAt 0)
                      (Space.endsWordAt (vLen - 1))
                  )
                  ss
            , endSpace =
                memberSatisfies
                  ( firstOnly
                      (Space.endsWordAt (vLen - 1))
                      (Space.startsWordAt 0)
                  )
                  ss
            }
    Nothing -> state

cost : BoundaryState -> Float
cost state =
  case state of
    Just b ->
      (if b.startSpace then spaceCost else 0.0) +
        (if b.endSpace then spaceCost else 0.0) +
        ( if b.startSpace && b.endSpace && not b.midSpace then wordCost
          else 0.0
        )
    Nothing -> 0.0

memberSatisfies : (a -> Bool) -> List a -> Bool
memberSatisfies predicate l = List.filter predicate l /= []

neither : (a -> Bool) -> (a -> Bool) -> a -> Bool
neither predicate1 predicate2 x = not (predicate1 x || predicate2 x)

firstOnly : (a -> Bool) -> (a -> Bool) -> a -> Bool
firstOnly predicate1 predicate2 x = predicate1 x && not (predicate2 x)
