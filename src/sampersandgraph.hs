import Data.List
import Data.Maybe
import Control.Monad

type Graph a = [(Node a,Edges)]
type Edges = [NodeID]
type NodeID = Int

data Node a = Node { getNodeID  :: NodeID,
                     getNodeVal :: a }
  deriving (Show,Eq)

nodeA,nodeB,nodeC :: Node Char
nodeA = Node 0 'A'
nodeB = Node 1 'B'
nodeC = Node 2 'C'
nodeD = Node 3 'D'


exGraph :: Graph Char
exGraph = [(nodeA,[1,2])
          ,(nodeB,[])
          ,(nodeC,[0,1])
          ,(nodeD,[])]

-- insertEdge :: (NodeID,NodeID) -> Graph a -> Maybe (Graph a)
-- insertEdge _ [] = Nothing
-- insertEdge (n1, n2) graph = do
--   src <- lookupNode n1 graph
--   guard $ isJust $ lookupNode n2 graph
--   let (lhs, (_, edges):rhs) = break (nodeEdgeIdEql n1) graph 
--     in Just $ lhs ++ (src, n2 : edges) : rhs
insertEdge :: (NodeID,NodeID) -> Graph a -> Maybe (Graph a)
insertEdge (n1, n2) graph
  | isNothing $ lookupNode n1 graph = Nothing
  | isNothing $ lookupNode n2 graph = Nothing
  | otherwise                       = Just $ helper (n1, n2) graph
  where
    helper :: (NodeID, NodeID) -> Graph a -> Graph a
    helper (n1, n2) ((node, edges):nodes)
      | getNodeID node == n1 = (node, n2:edges):nodes
      | otherwise            = (node, edges) : helper (n1, n2) nodes

lookupNode :: NodeID -> Graph a -> Maybe (Node a)
lookupNode nID graph = fst <$> find (nodeEdgeIdEql nID) graph

nodeEdgeIdEql :: NodeID -> (Node a, Edges) -> Bool
nodeEdgeIdEql nID (node, _) = nID == getNodeID node

main = print $ insertEdge (0, 3) exGraph

