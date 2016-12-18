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

fLabel : List String -> (List (Int, String), Array Node)
fLabel = List.foldl fLabelHelper ([], Array.repeat 1 [])

fLabelHelper :
  String -> (List (Int, String), Array Node) ->
    (List (Int, String), Array Node)
fLabelHelper path (paths, nodes) =
  case paths of
    (prevStart, prev) :: rest ->
      let (start, current) = getFork 0 path nodes in
        if start == prevStart then
          let
            (handle, prevTine, tine) = combineStart prev current
          in let
            (newNodes, newStart) = insertPath prevStart handle nodes
            newPaths = (newStart, tine) :: (newStart, prevTine) :: rest
          in
            (newPaths, newNodes)
        else ((start, current) :: paths, nodes) -- start < prevStart
    [] -> ([(0, path)], nodes)

combineStart : String -> String -> (String, String, String)
combineStart path1 path2 =
  case (String.uncons path1, String.uncons path2) of
    (Just (first1, rest1), Just (first2, rest2)) ->
      if first1 == first2 then
        let (handle, tine1, tine2) = combineStart rest1 rest2 in
          (String.cons first1 handle, tine1, tine2)
      else ("", path1, path2)
    _ -> ("", path1, path2)

getFork : Int -> String -> Array Node -> (Int, String)
getFork start path nodes =
  case Maybe.andThen (Array.get start nodes) List.head of
    Nothing -> (start, path)
    Just edge ->
      case String.uncons path of
        Nothing -> (start, path)
        Just (_, "") -> (start, path)
        Just (start, path) ->
          if edge.phoneme == phoneme then getFork edge.dst newPath nodes
          else (start, path)

insertPath : Int -> String -> Array Node -> (Array Node, Int)
insertPath start path nodes =
  case String.uncons path of
    Nothing -> (nodes, start)
    Just (first, rest) ->
      let next = Array.length nodes in
        insertPath
          next
          rest <|
          addEdge start first next <| Array.push [] nodes

addEdge : Int -> Char -> Int -> Array Node -> Array Node
addEdge src phoneme dst nodes =
  let node = Maybe.withDefault [] <| Array.get src nodes in
    Array.set src ({ phoneme = phoneme, dst = dst } :: node) nodes

reverseNodes : Array Node -> Array Node
reverseNodes
  Array.foldr addReverseEdges Array.empty
  List.foldl (addReverseEdge i) newNodes edges

bLabel : List (String, Int) -> (List (Int, String, Int), Array Node)
bLabel = List.foldl bLabelHelper ([], Array.initialize 1 <| always [])

addReverseEdges : Int -> List Edge -> Array Node -> Array Node
addReverseEdges edgeCount edges nodes =
  Array.push
    [] <|
    List.foldl (addReverseEdge edgeCount <| Array.length nodes) nodes edges

addReverseEdge : Int -> Edge -> Array Node -> Array Node
addReverseEdge edgeCount src edge =
  addEdge (edgeCount - 1 - edge.dst) edge.phoneme src
