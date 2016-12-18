module SuperDAG exposing
  (SuperDAG, SuperNode, SuperEdge, DAG, Node, Edge, toDAG, length)

import Array exposing (Array)
import List
import String

type alias SuperDAG = (SuperNode, List SuperNode)
type alias SuperNode =
  { edges : (SuperEdge, List SuperEdge), space : Bool, xPos : Float }
type alias SuperEdge = { phonemes : (Char, String), xStop : Float }
type alias DAG = Array Node
type alias Node = { edges : List Edge, space : Bool, xPos : Float }
type alias Edge = { phoneme : Char, dst : Int }

toDAG : SuperDAG -> DAG
toDAG dag = Array.initialize (length dag) <| toNode dag

length : SuperDAG -> Int
length (node, nodes) = List.sum <| List.map superNodeLength <| node :: nodes

superNodeLength : SuperNode -> Int
superNodeLength node =
  let (edge, edges) = node.edges in
    1 + (List.sum <| List.map superEdgeLength <| edge :: edges)

superEdgeLength : SuperEdge -> Int
superEdgeLength = String.length << snd << .phonemes

toNode : SuperDAG -> Int -> Node
toNode (node, nodes) i =
  if i <= 0 then
    if i == 0 then
      { edges = toEdges (node, nodes), space = node.space, xPos = node.xPos }
    else
      Debug.crash <|
        "negative index " ++ toString i ++ " while converting SuperDAG"
  else toNodeHelper (node, nodes) <| i - 1

toNodeHelper : SuperDAG -> Int -> Node
toNodeHelper (node, nodes) i =
  let (edge, edges) = node.edges in
    case String.uncons <| String.dropLeft i <| snd edge.phonemes of
      Just phonemes ->
        { edges =
            [ toEdge
                ( { node | edges = ({ edge | phonemes = phonemes }, edges) }
                , nodes
                )
            ]
        , space = False
        , xPos =
            lerp node.xStart edge.xStop (i + 1) <| superEdgeLength edge + 1
        }
      Nothing ->
        let newI = i - superEdgeLength edge in
          case (edges, nodes, newI) of
            (edge :: edges, _, _) ->
              toNodeHelper ({ node | edges = (edge, edges) }, nodes) newI
            ([], node :: nodes, 0) ->
              { edges = toEdges (node, nodes)
              , space = node.space
              , xPos = node.xPos
              }
            ([], node :: nodes, _) -> toNodeHelper (node, nodes) <| newI - 1
            ([], [], _) -> { edges = [], space = True, xPos = edge.xStop }

toEdges : SuperDAG -> List Edge
toEdges (node, nodes) =
  toEdge (node, nodes) ::
    case snd node.edges of
      (edge :: edges) -> toEdges ({ node | edges = (edge, edges) }, nodes)
      [] -> []

toEdge : SuperDAG -> Edge
toEdge (node, nodes) =
  let edge = fst node.edges in
    { phoneme = fst edge.phonemes
    , dst =
        if snd edge.phonemes == "" then
          List.sum <|
            List.map
              superNodeLength <|
              takeWhile ((>) edge.xStop << .xPos) <| node :: nodes
        else 1
    }

takeWhile : (a -> Bool) -> List a -> List a
takeWhile pred list =
  case list of
    [] -> []
    x :: xs -> if pred x then x :: takeWhile pred xs else []

lerp : Float -> Float -> Int -> Int -> Float
lerp start stop n i = (start * toFloat (n - i) + stop * toFloat i) / toFloat n


type alias Branch = { start : Float, phonemes : String, stop : Float }

type alias SuperNode = { start : Float, i : Int, edges : List SuperEdge }

type alias SuperEdge = { phoneme : Char, phonemes : String, stop : Float }

fromBranches : List Branch -> DAG
fromBranches branches =

type alias Branch = Maybe (Float, SuperEdge)

fromBranches : Int -> List Branch -> DAG
fromBranches iStart branches =
  let
    branches = List.filterMap identity branches
  in let
    xStop =
      Maybe.withDefault 0 <| List.maximum <| List.map (.stop << snd) branches
    superDAG = List.foldl addToGroup branches
  in let
    (xPosToI, iStop) =
      Dict.foldl insertSuperNodeI (Dict.empty, iStart) superDAG
  in let
    xPosToI = Dict.insert xStop iStop xPosToI
  in




toBranch : Float -> Float -> String -> Branch
toBranch start stop phonemes =
  case String.uncons phonemes of
    Nothing -> Nothing
    Just (phoneme, phonemes) ->
      Just (start, { phoneme = phoneme, phonemes = phonemes, stop = stop })

addToGroup : (k, v) -> Dict k (List v) -> Dict k (List v)
addToGroup (k, v) = Dict.update k (Just << (::) v << Maybe.withDefault [])

insertSuperNodeI :
  Float -> List SuperEdge -> (Dict Float Int, Int) -> (Dict Float Int, Int)
insertSuperNodeI xPos superNode (xPosToIndex, i) =
  ( Dict.insert xPos i xPosToIndex
  , i + 1 + (List.sum <| List.map (String.length << .phonemes) superNode
  )

addSuperNode : Dict Float Int -> Float -> List SuperEdge -> DAG -> DAG
addSuperNode xPosToI xPos superNode dag =
  let
    node =
      { edges = List.foldl
      , xPos = superNode.xPos
      }
  in let
    dag =
      Array.push
