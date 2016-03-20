module Space where

type alias Space = (Int, Int, Int, Int)

fromKIndex : Int -> Int -> Int -> Space
fromKIndex kLen vLen kIndex =
  ( getVStart kLen vLen <| kIndex - 1
  , getVEnd kLen vLen <| kIndex - 1
  , getVStart kLen vLen kIndex
  , getVEnd kLen vLen kIndex
  )

startsWordAt : Int -> Space -> Bool
startsWordAt vIndex (_, _, start, end) = start <= vIndex && vIndex <= end

endsWordAt : Int -> Space -> Bool
endsWordAt vIndex (start, end, _, _) = start <= vIndex && vIndex <= end

minus : Int -> Space -> Maybe Space
minus subtrahend (start1, end1, start2, end2) =
  let
    newRanges =
      ( max 0 <| start1 - subtrahend
      , max -1 <| end1 - subtrahend
      , max 0 <| start2 - subtrahend
      , max -1 <| end2 - subtrahend
      )
  in
    case newRanges of
      (0, -1, 0, -1) -> Nothing
      _ -> Just newRanges

getVStart : Int -> Int -> Int -> Int
getVStart kLen vLen kIndex =
  if kIndex > 0 && kIndex < kLen then
    max 0 <| min (vLen - 1) <| (kLen + vLen) // 2 - kLen + kIndex
  else 0

getVEnd : Int -> Int -> Int -> Int
getVEnd kLen vLen kIndex =
  if kIndex >= 0 && kIndex < kLen - 1 then
    max 0 <| min (vLen - 1) <| (kLen + vLen + 1) // 2 - kLen + kIndex
  else if kIndex == kLen - 1 then vLen - 1
  else -1
