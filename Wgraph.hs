module Wgraph
( fromLines,
  dijkstra,
  pathToNode,
  distToNode,
  edges,
  Node,
  Edge(..),
  Wedge(..),
  Wnode(..),
  Wgraph(..)
) where

import Data.List
import Data.Maybe
import Data.Function

-------------------------
-- TYPES AND CONSTRUCTORS
-------------------------
type Node = Int
data Edge = Edge (Node, Node) deriving (Show)
data Wedge = Wedge (Edge, Float) deriving (Show)
data Wnode = Wnode {node :: Node, pre :: Node, dist :: Float} deriving (Show, Eq)
data Wgraph = Wgraph [Wedge] deriving (Show)

-- Get a weighted graph from some lines of text, where each line specifies two nodes and a weight
fromLines :: [String] -> Wgraph
fromLines = Wgraph . map readData . map words
  where readData [n1, n2, w] = Wedge (Edge (read n1 :: Node, read n2 :: Node), read w :: Float)

-------------------
-- GENERAL PURPOSE
-------------------

-- List of weighted edges for a weighted graph
edges :: Wgraph -> [Wedge]
edges (Wgraph es) = es

-- List of nodes for a weighted edge
enodes :: Wedge -> [Node]
enodes (Wedge (Edge (n1, n2), weight)) = [n1, n2]

-- List of all nodes for a list of weighted edges
nodesForEdges :: [Wedge] -> [Node]
nodesForEdges = nub . concat . map enodes

-- List of all nodes for a graph
nodes :: Wgraph -> [Node]
nodes = nodesForEdges . edges

-- The weight of a weighted edge
weight :: Wedge -> Float
weight (Wedge (_, w)) = w

-- Given a node and a graph, get a list of weighted edges incident on the node
incidentWedges :: Node -> Wgraph -> [Wedge]
incidentWedges node = filter (\e -> node `elem` enodes e) . edges

-- Given a node and a list of weighted nodes, get the corresponding weighted node
wnodeForNode :: Node -> [Wnode] -> Wnode
wnodeForNode n = head . filter (\wn -> node wn == n)

-- Given a couple of nodes and a graph, try to find a weighted edge incident on the nodes
tryGetWedge :: Wgraph -> Node -> Node -> Maybe Wedge
tryGetWedge (Wgraph es) n1 n2  = find (\x -> [n1, n2] \\ enodes x == []) es

-- Given a weighted node and maybe a weighted edge, 
-- return the weighted node if we were given an edge and the node is in the edge -- otherwise Nothing
tryGetWnode :: Wnode -> Maybe Wedge -> Maybe Wnode
tryGetWnode wnode Nothing = Nothing
tryGetWnode wnode (Just wedge)
    | (node wnode) `elem` (enodes wedge) = Just wnode
    | otherwise = Nothing

-----------------
-- INITIALIZATION
-----------------

-- Initialize the weighted nodes and bootstrap the recursive algorithm
dijkstra :: Wgraph -> Node -> [Wnode]
dijkstra g start = 
  let wnodes = initWnodes start g
      curNode = Just (wnodeForNode start wnodes)
      checked = []
      (_, _, _, wnodes') = dijkstraAlg g checked curNode wnodes
      in wnodes'

-- Given a node, the start node and the graph, get a Wnode with the initial distance
wnodeForStart :: Node -> Node -> Wgraph -> Wnode
wnodeForStart node start graph
  | node == start = Wnode { node = node, pre = node, dist = 0 }
  | isNothing maybeWedge = Wnode { node = node, pre = start, dist = infinity}
  | otherwise = Wnode { node = node, pre = start, dist = weight . fromJust $ maybeWedge }
  where infinity = 1.0/0.0 :: Float
        maybeWedge = tryGetWedge graph node start

initWnodes :: Node -> Wgraph -> [Wnode]
initWnodes start graph = map (\n -> wnodeForStart n start graph) . nodes $ graph

-----------------
-- MAIN ALGORITHM
-----------------

-- Recursively perform Dijkstra's algorithm until all nodes have been checked
dijkstraAlg :: Wgraph -> [Node] -> Maybe Wnode -> [Wnode] -> (Wgraph, [Node], Maybe Wnode, [Wnode])
dijkstraAlg g checked Nothing wnodes = (g, checked, Nothing, wnodes)
dijkstraAlg g checked (Just curNode) wnodes =
      let cn = node curNode
          checked' = cn : checked
          incidents = incidentWedges cn g
          wnodes' = updateWnodes g curNode incidents wnodes
          unChecked = unchecked checked' wnodes'
          curNode' = minimalUnchecked unChecked
      in dijkstraAlg g checked' curNode' wnodes'

-- Given a list of Wnodes and a list of checked nodes, return a list of unchecked Wnodes
unchecked :: [Node] -> [Wnode] -> [Wnode]
unchecked checked wnodes = filter (\w -> not $ (node w) `elem` checked) wnodes

-- Given a list of unchecked Wnodes, maybe return the minimal weighted node
minimalUnchecked :: [Wnode] -> Maybe Wnode
minimalUnchecked [] = Nothing
minimalUnchecked wnodes = Just . minimumBy (compare `on` dist) $ wnodes

-- Given a base node and the weighted edges incident on it, Update the list of weighted nodes
updateWnodes :: Wgraph -> Wnode -> [Wedge] -> [Wnode] -> [Wnode]
updateWnodes g curNode incidents = map (updateWnode g curNode incidents)

-- Given a base node, the edges incident on it and a weighted node, return a (possibly) updated weighted node
updateWnode :: Wgraph -> Wnode -> [Wedge] -> Wnode -> Wnode
updateWnode g curNode incidents wnode
  | tryGetWnode wnode mWedge == Nothing = wnode 
  | otherwise = updateConnected curNode mWedge wnode
  where mWedge = tryGetWedge g (node curNode) (node wnode)

-- Given a base node an edge and a connected weighted node, return an updated weighted node
updateConnected :: Wnode -> Maybe Wedge -> Wnode -> Wnode
updateConnected curNode (Just wedge) wnode = 
  let ext = (dist curNode) + (weight wedge)
      improved = ext < (dist wnode)
  in Wnode {
      node = node wnode,
      pre = if improved then node curNode else pre wnode,
      dist = min ext (dist wnode)
    }

----------------------
-- EXTRACTING RESULTS
----------------------

-- Return a path to a node as a list
pathToNode :: [Wnode] -> Node -> [Node]
pathToNode wnodes start = reverse . map (node) . pathToWnode wnodes . wnodeForNode start $ wnodes

-- Return a path to a weighted node
pathToWnode :: [Wnode] -> Wnode -> [Wnode]
pathToWnode wnodes wnode 
  | node wnode == pre wnode = [wnode]
  | otherwise = wnode : pathToWnode wnodes prenode'
  where prenode' = wnodeForNode (pre wnode) wnodes

distToNode :: [Wnode] -> Node -> Float
distToNode wnodes node = dist $ wnodeForNode node wnodes
