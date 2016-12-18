module CombineEnds exposing (combineEnds, combineEnds3)

combineEnds : String -> String -> (String, String, String, String)
combineEnds a b =
  let
    (l, a, b) = combineLeftHelper 0 a b
  in let
    (a, b, r) = combineRightHelper 0 a b
  in
    (l, a, b, r)

combineLeftHelper : Int -> String -> String -> (String, String, String)
combineLeftHelper lLength a b =
  let
    aNew = String.dropLeft lLength a
    bNew = String.dropLeft lLength b
  in
    if
      min (String.length aNew) (String.length bNew) > 0 &&
        String.left 1 aNew == String.left 1 bNew
    then combineLeftHelper (lLength + 1) a b
    else (String.left lLength a, aNew, bNew)

combineRightHelper : Int -> String -> String -> (String, String, String)
combineRightHelper rLength a b =
  let
    aNew = String.dropRight rLength a
    bNew = String.dropRight rLength b
  in
    if
      min (String.length aNew) (String.length bNew) > 1 &&
        String.right 1 aNew == String.right 1 bNew
    then combineRightHelper (rLength + 1) a b
    else (aNew, bNew, String.right rLength a)

combineEnds3 :
  String -> String -> String -> (String, String, String, String, String)
combineEnds3 a b c =
  let
    (l, a, b, c) = combineLeft3Helper 0 a b
  in let
    (a, b, c, r) = combineRight3Helper 0 a b
  in
    (l, a, b, c, r)

combineLeft3Helper :
  Int -> String -> String -> String -> (String, String, String, String)
combineLeft3Helper lLength a b c =
  let
    aNew = String.dropLeft lLength a
    bNew = String.dropLeft lLength b
    cNew = String.dropLeft lLength c
  in
    if
      minLength [aNew, bNew, cNew] > 0 &&
        String.left 1 aNew == String.left 1 bNew &&
        String.left 1 bNew == String.left 1 cNew
    then combineLeft3Helper (lLength + 1) a b c
    else (String.left lLength a, aNew, bNew, cNew)

combineRight3Helper :
  Int -> String -> String -> String -> (String, String, String, String)
combineRight3Helper rLength a b c =
  let
    aNew = String.dropRight rLength a
    bNew = String.dropRight rLength b
    cNew = String.dropRight rLength c
  in
    if
      minLength [aNew, bNew, cNew] > 1 &&
        String.right 1 aNew == String.right 1 bNew &&
        String.right 1 bNew == String.right 1 cNew
    then combineRight3Helper (rLength + 1) a b c
    else (aNew, bNew, cNew, String.right rLength a)

minLength : List String -> Int
minLength = Maybe.withDefault 0 << List.minimum << List.map String.length
