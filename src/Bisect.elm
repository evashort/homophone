module Bisect exposing (bisectLeft, bisectRight)

import Array exposing (Array)

bisectLeft : comparable -> Array comparable -> Int
bisectLeft key a = bisectRangeLeft key a 0 <| Array.length a

bisectRight : comparable -> Array comparable -> Int
bisectRight key a = bisectRangeRight key a 0 <| Array.length a

-- returns the first possible insertion index in [start, stop]
bisectRangeLeft : comparable -> Array comparable -> Int -> Int -> Int
bisectRangeLeft key a start stop =
  if start >= stop then start
  else
    let mid = (start + stop) // 2 in
      let tooLow =
        case Array.get mid a of
          Nothing -> False
          Just guess -> guess < key
      in
        if tooLow then bisectRangeLeft key a (mid + 1) stop
        else bisectRangeLeft key a start mid

-- returns the last possible insertion index in [start, stop]
bisectRangeRight : comparable -> Array comparable -> Int -> Int -> Int
bisectRangeRight key a start stop =
  if start >= stop then start
  else
    let mid = (start + stop) // 2 in
      let tooHigh =
        case Array.get mid a of
          Nothing -> False
          Just guess -> guess > key
      in
        if tooHigh then bisectRangeRight key a start mid
        else bisectRangeRight key a (mid + 1) stop
