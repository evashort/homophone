import CombineEnds

-- non-empty string
type alias NString = (Char, String)

length : NString -> Int
length = (+) 1 << String.length << snd

toSuperDAG : List String -> Maybe SuperDAG
toSuperDAG pathList =
  let
    width = List.maximum <| List.map String.length pathList
  in let
    fWidth = toFloat width
  in
    case pathList of
      [a] ->
        case String.uncons a of
          Just a ->
            Just
              { nodes =
                  [ { edges = [{ phonemes = a, xStop = fWidth }]
                    , space = True
                    , xPos = 0
                    }
                  ]
              , xStop = fWidth
              }
          _ -> Nothing
      [a, b] ->
        let
          (l, a, b, r) = combineEnds a b
        in let
          lStop = toFloat <| String.length l
          rStart = toFloat <| width - String.length r
        in
          case (String.uncons a, String.uncons b) of
            (Just a, Just b) ->
              Just
                { nodes =
                    addSpace <|
                      List.filterMap identity
                        [ fromTrunk 0 lStop l
                        , fromTwigs lStop rStart a b
                        , fromTrunk rStart fWidth r
                        ]
                , xStop = fWidth
                }
            _ -> Nothing
      [a, b, c] ->
        let
          (a, b) =
            if String.length b > String.length a then (b, a) else (b, a)
        in let
          (a, c) =
            if String.length c > String.length a then (c, a) else (c, a)
        in let
          (l, a, b, c, r) = CombineEnds.combineEnds3 a b c
        in let
          (lab, a, b, rab) = CombineEnds.combineEnds a b
        in let
          (lac, a, c, rac) = CombineEnds.combineEnds a c
        in let
          (lbc, b, c, rbc) = CombineEnds.combineEnds b c
        in let
          lStop = String.length l
          rStart = width - String.length r
          lbcLength = String.length lbc
          rbcLength = String.length rbc
        in let
          labStop = lStop + String.length lab
          rabStart = rStart - String.length rab
          lacStop = lStop + String.length lac
          racStart = rStart - String.length rac
        in let
          (dStart, dStop, dLength) =
            densityMax
              (labStop, rabStart, lbcLength + String.length b + rbcLength)
              (lacStop, racStart, lbcLength + String.length c + rbcLength)
        in let
          bStart = iLerp labStop dStop dLength lbcLength
          bStop = iLerp rabStart dStart dLength rbcLength
          cStart = iLerp lacStop dStop dLength lbcLength
          cStop = iLerp racStart dStart dLength rbcLength
          lStop = toFloat lStop
          rStart = toFloat rStart
          labStop = toFloat labStop
          rabStart = toFloat rabStart
          lacStop = toFloat lacStop
          racStart = toFloat racStart
        in let
          aStart = max labStop lacStop
          aStop = min rabStart racStart
          lbcStop = min bStart cStart
          rbcStart = max bStop cStop
        in
          case (String.uncons a, String.uncons b, String.uncons c) of
            (Just a, Just b, Just c) ->
              Just
                { nodes =
                    addSpace <|
                      List.map toNode <|
                        Dict.toList groupByKey <|
                          List.filterMap identity
                            [ toEdge 0 lStop l
                            , toEdge lStop labStop lab
                            , toEdge lStop lacStop lac
                            , toEdge lStop lbcStop lbc
                            , Just (aStart, { phonemes = a, xStop = aStop })
                            , Just (bStart, { phonemes = b, xStop = bStop })
                            , Just (cStart, { phonemes = c, xStop = cStop })
                            , toEdge rabStart rStart rab
                            , toEdge racStart rStart rac
                            , toEdge rbcStart rStart rbc
                            , toEdge rStart fWidth r
                            ]
                , xStop = fWidth
                }
            _ -> Nothing
      [a, b, c, d] ->
        let
          (lab, ab, rab) = CombineEnds.combineEnds a b
          (lcd, cd, rcd) = CombineEnds.combineEnds c d
        in let
          (l, la, lb, m, ra, rb, r) =
            case (lab == lcd, rab = rcd) of
              (False, True) ->
                let (l, la, lb, m) = CombineEnds.combineEnds lab lcd in
                  (l, la, lb, m, ab, cd, rab)
              (True, False) ->
                let (m, ra, rb, r) = CombineEnds.combineEnds rab rcd in
                  (lab, ab, cd, m, ra, rb, r)
              _ -> Nothing
        in let
          lStop = String.length l
        in let
          mStart = lStop + max (String.length la) (String.length lb)
        in let
          lStop = toFloat lStop
          mStart = toFloat mStart
          mStop = toFloat <| mStart + String.length m
          rStart = toFloat <| width - String.length r
        in
          case
            ( String.uncons la
            , String.uncons lb
            , String.uncons ra
            , String.uncons rb
            )
          of
            (Just la, Just lb, Just ra, Just rb) ->
              Just
                { nodes =
                    addSpace <|
                      List.filterMap identity
                        [ fromTrunk 0 lStop l
                        , fromTwigs lStop mStart la lb
                        , fromTrunk mStart mStop m
                        , fromTwigs mStop rStart ra rb
                        , fromTrunk rStart fWidth r
                        ]
                , xStop = fWidth
                }
            _ -> Nothing
      _ -> Nothing

addSpace : List SuperNode -> List SuperNode
addSpace nodes =
  case nodes of
    [] -> []
    node :: nodes -> { node | space = True } :: nodes

fromTrunk : Float -> Float -> String -> Maybe SuperNode
fromTrunk xStart xStop trunk =
  case String.uncons trunk of
    Just phonemes ->
      Just
        { edges = [{ phonemes = phonemes, xStop = xStop }]
        , space = False
        , xPos = xStart
        }
    Nothing -> Nothing

fromTwigs : Float -> Float -> NString -> NString -> Maybe SuperNode
fromTwigs xStart xStop aTwig bTwig =
  Just
    { edges =
        [ { phonemes = aTwig, xStop = xStop }
        , { phonemes = bTwig, xStop = xStop }
        ]
    , space = False
    , xPos = xStart
    }

toEdge : Float -> Float -> String -> Maybe SuperEdge
toEdge xStart xStop phonemes
  case String.uncons phonemes of
    Just phonemes -> Just (xStart, { phonemes = phonemes, xStop = xStop })
    Nothing -> Nothing

toNode : (Float, (SuperEdge, List SuperEdge)) -> SuperNode
toNode (xStart, edge, edges) =
  { edges = edge :: edges, space = false, xPos = xStart }

groupByKey : List (k, v) -> Dict k (v, List v)
groupByKey xs =
  case xs of
    [] -> Dict.empty
    (k, v) :: xs -> Dict.update k (addToGroup v) <| groupBy pred xs

addToGroup : v -> Maybe (v, List v) -> Maybe (v, List v)
addToGroup x xs =
  Just <|
    case xs of
      Nothing -> (x, [])
      Just (y, xs) -> (x, y :: xs)

densityMax : (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
densityMax (aStart, aStop, aLength) (bStart, bStop, bLength) =
  if bLength * (aStop - aStart) > aLength * (bStop - bStart) then
    (bStart, bStop, bLength)
  else (aStart, aStop, aLength)

iLerp : Int -> Int -> Int -> Int -> Float
iLerp start stop n i = toFloat (start * (n - i) + stop * i) / toFloat n
