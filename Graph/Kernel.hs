{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,FlexibleInstances, FlexibleContexts #-}

-- Kernel.hs
-- Mathijs Saey
-- This module contains the basic Graph class

module Graph.Kernel where
import Data.Maybe

-- g is the global type of the graph
-- n is the type of the node labels (must be unique)
-- e is the type of the edge labels
class Graph g n e | g -> n e where
	insertNode 	:: g -> n -> g 					-- add a node to the given graph
	insertEdge 	:: g -> (n,n) -> e -> g 		-- add an edge to the given graph
	nodes 		:: g -> [n] 					-- enumeration of the nodes
	empty 		:: g 							-- returns an empty graph 
	edge 		:: g -> (n,n) -> Maybe e 		-- returns an edge between two nodes (if any)

	-- Gets all the nodes reachable from this node
	getNeighbours :: (Eq e, Eq n) => g -> n -> [n]
	getNeighbours g n = [i | i <- nodes g, i /= n, edge g (n, i) /= Nothing]

	-- Gets all the nodes that can lead to this node
	getToNodes :: (Eq e, Eq n) => g -> n -> [n]
	getToNodes g n = [i | i <- nodes g, i /= n, edge g (i, n) /= Nothing]

	-- Gets the weight of a path, assumes the path exists
	getPathWeight :: (Num e) => g -> [n] -> e
	getPathWeight g [] = 0
	getPathWeight g ls = fst $ foldl (\(w, p) x -> (((fromJust (edge g (p,x))) + w), x))
									 (0, head ls) (tail ls)


