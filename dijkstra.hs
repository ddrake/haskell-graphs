import Data.List
import Data.Maybe
import Data.Function

type Node = Int
data Edge = Edge (Node, Node) deriving (Show)
data Wedge = Wedge (Edge, Float) deriving (Show)
data Wnode = Wnode {node :: Node, pre :: Node, dist :: Float} deriving (Show, Eq)
data Wgraph = Wgraph [Wedge] deriving (Show)

fromString :: String -> Wgraph
fromString s = Wgraph . map parse . map words . lines $ s
  where parse [n1, n2, w] = Wedge (Edge (read n1 :: Node, read n2 :: Node), read w :: Float)

fromList :: [((Node, Node), Float)] -> Wgraph
fromList = Wgraph . map (\((n1, n2), w) -> Wedge (Edge (n1,n2), w))

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

-- Given a couple of nodes and a graph, try to find a weighted edge incident on the nodes
tryGetWedge :: Wgraph -> Node -> Node -> Maybe Wedge
tryGetWedge (Wgraph es) n1 n2  = find (\x -> [n1, n2] \\ enodes x == []) es

-- Given a weighted node and maybe a weighted edge, return the weighted node if its node is in the edge or nothing if not
tryGetWnode :: Wnode -> Maybe Wedge -> Maybe Wnode
tryGetWnode wnode Nothing = Nothing
tryGetWnode wnode (Just wedge)
    | (node wnode) `elem` (enodes wedge) = Just wnode
    | otherwise = Nothing

-- Given a node, the start node and the graph, get a Wnode with the initial distance
wnodeForStart :: Node -> Node -> Wgraph -> Wnode
wnodeForStart node start graph
  | node == start = Wnode { node = node, pre = node, dist = 0 }
  | isNothing maybeWedge = Wnode { node = node, pre = start, dist = 1000}
  | otherwise = Wnode { node = node, pre = start, dist = weight . fromJust $ maybeWedge }
  where maybeWedge = tryGetWedge graph node start

initWnodes :: Node -> Wgraph -> [Wnode]
initWnodes start graph = map (\n -> wnodeForStart n start graph) . nodes $ graph

-- Given a list of Wnodes and a list of checked nodes, return a list of unchecked Wnodes
unchecked :: [Wnode] -> [Node] -> [Wnode]
unchecked wnodes checked = filter (\wn -> not $ (node wn) `elem` checked) wnodes

-- Given a list of unchecked Wnodes, maybe return the minimal weighted node
minimalUnchecked :: [Wnode] -> Maybe Wnode
minimalUnchecked [] = Nothing
minimalUnchecked wnodes = Just . minimumBy (compare `on` dist) $ wnodes

-- Given a node and a graph, get a list of weighted edges incident on the node
incidentWedges :: Node -> Wgraph -> [Wedge]
incidentWedges node = filter (\e -> node `elem` enodes e) . edges

-- Given a node and a list of weighted nodes, get the corresponding weighted node
wnodeForNode :: Node -> [Wnode] -> Wnode
wnodeForNode n = head . filter (\wn -> node wn == n)

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

-- Recursively perform Dijkstra's algorithm until all nodes have been checked
dijkstraAlg :: Wgraph -> [Node] -> Maybe Wnode -> [Wnode] -> (Wgraph, [Node], Maybe Wnode, [Wnode])
dijkstraAlg g checked Nothing wnodes = (g, checked, Nothing, wnodes)
dijkstraAlg g checked (Just curNode) wnodes =
      let checked' = node curNode : checked
          incidents = incidentWedges (node curNode) g
          wnodes' = updateWnodes g curNode incidents wnodes
          unChecked = unchecked wnodes' checked'
          curNode' = minimalUnchecked unChecked
      in dijkstraAlg g checked' curNode' wnodes'

-- Initialize the weighted nodes and bootstrap the recursive algorithm
dijkstraMain :: Wgraph -> Node -> (Wgraph, [Node], Maybe Wnode, [Wnode]) 
dijkstraMain g start = 
  let wnodes = initWnodes start g
      curNode = Just (wnodeForNode start wnodes)
      checked = []
  in dijkstraAlg g checked curNode wnodes

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

-- create a graph for testing...
-- let g = fromList [((1,2),3),((1,5),2),((1,6),9),((2,3),4),((2,6),2),((3,4),1),((3,6),3),((3,7),2),((4,7),2),((5,6),6),((6,7),1)]
