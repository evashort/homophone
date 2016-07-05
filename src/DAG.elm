module DAG exposing
  ( DAG, Node, Edge, PathList, get, length, wordAt, wordStart, empty
  , fromPathLists
  )

import Array exposing (Array)
import Bisect
import CBool exposing (CBool)
import Dict exposing (Dict)
import PairingHeap exposing (PairingHeap)
import Set exposing (Set)
import String

-- for efficiency, the set of pronunciations for each word is collapsed into a
-- Directed Acyclic Graph, where each edge is labelled with a phoneme and each
-- path through the DAG is a pronunciation. for example, the word wanted,
-- which can be pronounced wantud, wontid, or wonid, becomes
--          .___.___.
--        a/  n   t  \u
--   .___./        ___\.___.
--     w  \       /i  /  d
--        o\.___./__./i         (dots are nodes, labelled lines are edges)
--            n   t
--
-- this reduces duplicate work when most of the phonemes are shared between
-- pronunciations.

type alias DAG =
  { nodes : Array Node
  , spaces : Array Int
  }

type alias Node = List Edge

type alias Edge =
  { phoneme : Char
  , dst : Int
  }

type alias PathList = List String

type alias LooseEnd =
  { src : Int
  , phoneme : Char
  }

get : Int -> DAG -> Node
get i dag = Maybe.withDefault [] <| Array.get i dag.nodes

length : DAG -> Int
length = Array.length << .nodes

-- index of the word to the right of the given node
wordAt : Int -> DAG -> Int
wordAt nodeIndex dag = Bisect.bisectRight nodeIndex dag.spaces - 1

wordStart : Int -> DAG -> Maybe Int
wordStart i dag = Array.get i dag.spaces

empty : DAG
empty = { nodes = Array.fromList [[]], spaces = Array.fromList [0] }

fromPathLists : List PathList -> DAG
fromPathLists pathLists =
  List.foldl appendPathList empty pathLists

appendPathList : PathList -> DAG -> DAG
appendPathList pathList dag =
  let
    pathSets = List.foldl insertPath Dict.empty pathList
  in let
    fringe = Dict.foldl
      (insertLooseEnd (Array.length dag.nodes - 1))
      PairingHeap.empty
      pathSets
  in let
    newNodes = appendPathListHelper fringe dag.nodes
  in
    { nodes = newNodes
    , spaces = Array.push (Array.length newNodes - 1) dag.spaces
    }

appendPathListHelper :
  PairingHeap (Int, PathList) LooseEnd -> Array Node -> Array Node
appendPathListHelper fringe nodes =
  case getLooseEnds fringe of
    Nothing -> nodes
    Just (pathList, looseEnds, fringe2) ->
      let
        nodes2 = List.foldl tieUpLooseEnd nodes looseEnds
        pathSets = List.foldl insertPath Dict.empty pathList
      in let
        fringe3 = Dict.foldl
          (insertLooseEnd <| Array.length nodes2)
          fringe2
          pathSets
        nodes3 = Array.push [] nodes2
      in
        appendPathListHelper fringe3 nodes3

getLooseEnds :
  PairingHeap (Int, PathList) LooseEnd ->
    Maybe (PathList, List LooseEnd, PairingHeap (Int, PathList) LooseEnd)
getLooseEnds fringe =
  case PairingHeap.findMin fringe of
    Nothing -> Nothing
    Just ((_, pathList), _) -> Just <| getLooseEndsHelper [] pathList fringe

getLooseEndsHelper :
  List LooseEnd -> PathList -> PairingHeap (Int, PathList) LooseEnd ->
    (PathList, List LooseEnd, PairingHeap (Int, PathList) LooseEnd)
getLooseEndsHelper looseEnds pathList fringe =
  case PairingHeap.findMin fringe of
    Nothing -> (pathList, looseEnds, fringe)
    Just ((_, nextPathList), looseEnd) ->
      if nextPathList == pathList then
        getLooseEndsHelper
          (looseEnd :: looseEnds)
          pathList <|
          PairingHeap.deleteMin fringe
      else
        (pathList, looseEnds, fringe)

insertPath :
  String -> Dict (Char, CBool) (Set String) -> Dict (Char, CBool) (Set String)
insertPath pathString pathSets =
  case String.uncons pathString of
    Nothing -> pathSets
    Just (c, rest) ->
      let key = (c, CBool.cBool <| rest == "") in
        let pathSet = Maybe.withDefault Set.empty <| Dict.get key pathSets in
          Dict.insert key (Set.insert rest pathSet) pathSets

insertLooseEnd :
  Int -> (Char, CBool) -> Set String -> PairingHeap (Int, PathList) LooseEnd ->
  PairingHeap (Int, PathList) LooseEnd
insertLooseEnd i (c, _) pathSet fringe =
  let pathList = Set.toList pathSet in
    PairingHeap.insert
      ((pathListKey pathList, pathList), { src = i, phoneme = c })
      fringe

pathListKey : PathList -> Int
pathListKey pathList =
  negate <|
    Maybe.withDefault 0 <| List.maximum <| List.map String.length pathList

tieUpLooseEnd : LooseEnd -> Array Node -> Array Node
tieUpLooseEnd looseEnd nodes =
  case Array.get looseEnd.src nodes of
    Nothing -> Debug.crash "loose end src out of range"
    Just node ->
      Array.set
        looseEnd.src
        ({ phoneme = looseEnd.phoneme, dst = Array.length nodes } :: node)
        nodes
