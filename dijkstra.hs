import Data.List
import Data.Maybe
import Data.Function

type Node = Int
data Edge = Edge (Node, Node) deriving (Show)
data Wedge = Wedge (Edge, Float) deriving (Show)
data Wnode = Wnode {node :: Node, pre :: Node, dist :: Float} deriving (Show, Eq)
data Wgraph = Wgraph [Wedge] deriving (Show)

fromList :: [((Node, Node), Float)] -> Wgraph
fromList list = Wgraph $ map (\((n1, n2), w) -> Wedge (Edge (n1,n2), w)) list

-- List of weighted edges for a weighted graph
edges :: Wgraph -> [Wedge]
edges (Wgraph es) = es

-- List of nodes for a weighted edge
enodes :: Wedge -> [Node]
enodes (Wedge (Edge (n1, n2), weight)) = [n1, n2]

-- List of all nodes for a list of weighted edges
nodesForEdges :: [Wedge] -> [Node]
nodesForEdges wedges = nub . concat . map enodes $ wedges

-- List of all nodes for a graph
nodes :: Wgraph -> [Node]
nodes graph = nodesForEdges $ edges graph 

-- The weight of a weighted edge
weight :: Wedge -> Float
weight (Wedge (_, w)) = w

-- The edge of a weighted edge
edge :: Wedge -> Edge
edge (Wedge (e, _)) = e

-- Given a couple of nodes and a graph, try to find a weighted edge incident on the nodes
tryGetWedge :: Wgraph -> Node -> Node -> Maybe Wedge
tryGetWedge (Wgraph es) n1 n2  = find (\x -> [n1, n2] \\ enodes x == []) es

-- Given a node, the start node and the graph, get a Wnode with the initial distance
wnodeForStart :: Node -> Node -> Wgraph -> Wnode
wnodeForStart node start graph
  | node == start = Wnode { node = node, pre = node, dist = 0 }
  | isNothing maybeWedge = Wnode { node = node, pre = start, dist = 1000}
  | otherwise = Wnode { node = node, pre = start, dist = weight (fromJust maybeWedge) }
  where maybeWedge = tryGetWedge graph node start

initWnodes :: Node -> Wgraph -> [Wnode]
initWnodes start graph = map (\n -> wnodeForStart n start graph) (nodes graph)

-- Given a list of Wnodes and a list of checked nodes, return a list of unchecked Wnodes
unchecked :: [Wnode] -> [Node] -> [Wnode]
unchecked wnodes checked = filter (\wn -> not ((node wn) `elem` checked)) wnodes

-- Given a list of unchecked Wnodes, return the minimal weighted node
minimalUnchecked :: [Wnode] -> Maybe Wnode
minimalUnchecked [] = Nothing
minimalUnchecked wnodes = Just . minimumBy (compare `on` dist) $ wnodes

-- Given a node and the graph, get a list of weighted edges incident on the node
incidentWedges :: Node -> Wgraph -> [Wedge]
incidentWedges node graph = filter (\e -> node `elem` enodes e) . edges $ graph

-- Given a weighted edge and a node, get the opposite node
oppositeNode :: Wedge -> Node -> Node
oppositeNode (Wedge (Edge (n1, n2), _)) node = if n1 == node then n2 else n1

-- Given a node and a list of weighted nodes, get the corresponding weighted node
wnodeForNode :: Node -> [Wnode] -> Wnode
wnodeForNode n wnodes = head . filter (\wn -> node wn == n) $ wnodes

-- Given a base node and the weighted edges incident on it, Update the list of weighted nodes
updateWnodes :: Wgraph -> Wnode -> [Wedge] -> [Wnode] -> [Wnode]
updateWnodes g curNode incidents wnodes = map (updateWnode g curNode incidents) wnodes

-- Given a base node, the edges incident on it and a weighted node, return a (possibly) updated weighted node
updateWnode :: Wgraph -> Wnode -> [Wedge] -> Wnode -> Wnode
updateWnode g curNode incidents wnode
  | curNode == wnode = curNode
  | node wnode `elem` (nodesForEdges incidents) = 
    let n = node wnode
        cn = node curNode
        origp = pre wnode
        ext = ((dist curNode) + (weight . fromJust . tryGetWedge g n $ cn))
        improved = ext > dist wnode
    in Wnode {
        node = n,
        pre = if improved then cn else origp, 
        dist = min ext (dist wnode)
      }
  | otherwise = wnode


dijkstraAlg :: Wgraph -> [Node] -> Maybe Wnode -> [Wnode] -> (Wgraph, [Node], Maybe Wnode, [Wnode])
dijkstraAlg g checked Nothing wnodes = (g, checked, Nothing, wnodes)
dijkstraAlg g checked (Just curNode) wnodes =
      let checked' = nub (node curNode : checked)
          incidents = incidentWedges (node curNode) g
          wnodes' = updateWnodes g curNode incidents wnodes
          unChecked = unchecked wnodes' checked'
          curNode' = minimalUnchecked unChecked
      in dijkstraAlg g checked' curNode' wnodes'

dijkstraMain :: Wgraph -> Node -> (Wgraph, [Node], Maybe Wnode, [Wnode]) 
dijkstraMain g start = 
  let wnodes = initWnodes start g
      curNode = Just (wnodeForNode start wnodes)
      checked = []
  in dijkstraAlg g checked curNode wnodes


-- Algorithm
-- Create a graph 
--    let g = Wgraph [Wedge(Edge (1,2), 3), Wedge(Edge (2,3),4), Wedge(Edge (1,4),12)]
-- Create an empty list of checked nodes
--    let checked = []
-- Choose a start node
--    let start = 1 :: Node
-- Initialize the list of weighted nodes based on the start node selected
--    let wnodes = initWnodes start g

-- Select the minimal un-checked node (if none we're done)
--    let maybeNode = minimalUnchecked (unchecked wnodes checked)
--    let curNode = fromJust maybeNode
-- Add the node to the checked list
--    let checked' = node curNode : checked
-- Find all weighted edges incident on the node
--    let incidents = incidentWedges (node curNode) g
-- Update the list of weighted nodes
--    let wnodes' = updateWnodes g curNode incidents wnodes


-- let g = fromList [((1,2),3),((1,5),2),((1,6),9),((2,3),4),((2,6),2),((3,4),1),((3,6),3),((3,7),2),((4,7),2),((5,6),6),((6,7),1)]
