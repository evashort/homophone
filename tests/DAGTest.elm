module DAGTest exposing (dagTest)

import Array exposing (Array)
import Dict exposing (Dict)
import ElmTest exposing (..)
import List

import DAG exposing (DAG, Edge)

dagTest : Test
dagTest =
  suite "DAG Suite"
    [ test
        "It can merge redundant branches of the DAG" <|
        assertEqual
          (flattenDAG <| DAG.fromPathLists [["abc", "anz", "xyz"]]) <|
          [ ('a', ['b', 'n'])
          , ('b', ['c'])
          , ('c', [])
          , ('n', ['z'])
          , ('x', ['y'])
          , ('y', ['z'])
          , ('z', [])
          ]
    , test
        "It can merge different branches of the subtree rooted at 'a' at different times" <|
        assertEqual
          (flattenDAG <| DAG.fromPathLists [["axyz", "az", "bxyz"]]) <|
          [ ('a', ['x', 'z'])
          , ('b', ['x'])
          , ('x', ['y'])
          , ('y', ['z'])
          , ('z', [])
          ]
    ]

flattenDAG : DAG -> List (Char, List Char)
flattenDAG dag =
  Dict.toList <|
    Dict.map -- wow so dense
      (always <| List.sort << List.map .phoneme << lookup dag.nodes) <|
      Array.foldl (flip <| List.foldl insertEdge) Dict.empty dag.nodes

insertEdge : Edge -> Dict Char Int -> Dict Char Int
insertEdge edge dict =
  let oldDst = Dict.get edge.phoneme dict in
    if oldDst == Nothing then Dict.insert edge.phoneme edge.dst dict
    else if oldDst == Just edge.dst then dict
    else
      Debug.crash <|
        "two edges labeled " ++ toString edge.phoneme ++
          " lead to different nodes"

lookup : Array a -> Int -> a
lookup a i =
  case Array.get i a of
    Just x -> x
    Nothing ->
      Debug.crash <|
        "index " ++ toString i ++ " out of bounds for " ++ toString a
