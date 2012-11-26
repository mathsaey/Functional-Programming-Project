{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,FlexibleInstances, FlexibleContexts, DatatypeContexts #-}

-- Kernel.hs
-- Mathijs Saey
-- This module contains the basic Graph class and the Node and Edge data

module Graph.Kernel where

data (Read n) => Node n = No n deriving (Read, Show)
data (Read e) => Edge e = NoEdge | Ed e deriving (Read, Show)

-- g is the global type of the graph
-- n is the type of the node labels (must be unique)
-- e is the type of the edge labels
class Graph g n e | g -> n e where
	insertNode 	:: g -> n -> g 					-- add a node to the given graph
	insertEdge 	:: g -> (n,n) -> e -> g 		-- add an edge to the given graph
	nodes 		:: g -> [n] 					-- enumeration of the nodes
	empty 		:: g 							-- returns an empty graph 
	edge 		:: g -> (n,n) -> Maybe e 		-- returns an edge between two nodes (if any)
