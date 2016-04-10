module DAG where

import Array exposing (Array)
import Bisect
import CBool exposing (CBool)
import Dict exposing (Dict)
import PairingHeap exposing (PairingHeap)
import Set exposing (Set)
import String

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

isSpace : Int -> DAG -> Bool
isSpace i dag =
  Maybe.withDefault
    False <|
    Maybe.map
      ((==) i) <|
      Array.get (Bisect.bisectLeft i dag.spaces) dag.spaces

spaceInRange : Int -> Int -> DAG -> Bool
spaceInRange start end dag =
  Bisect.bisectLeft start dag.spaces < Bisect.bisectRight end dag.spaces

getSpace : Int -> DAG -> Maybe Int
getSpace i dag = Array.get i dag.spaces

fromPathLists : List PathList -> DAG
fromPathLists pathLists =
  List.foldl
    appendPathList
    { nodes = Array.fromList [[]], spaces = Array.fromList [0] }
    pathLists

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
