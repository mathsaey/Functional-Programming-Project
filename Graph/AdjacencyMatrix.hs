{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- AdjacencyMatrix.hs
-- Mathijs Saey
-- This module contains the AdjacenyMatrix graph implementation

module Graph.AdjacencyMatrix (AdjacencyMatrix, insertEdge, insertNode, nodes, edge, empty) where

import Graph.Kernel hiding (Node, No)
import Data.Array
import Data.List

type Node = Int
data AdjacencyMatrix e = Empty | AM (Array (Int, Int) (Edge e)) deriving Show

instance (Read e) => Graph (AdjacencyMatrix e) Node e where
	insertEdge g (n1 ,n2) e = insertEdge' g (n1,n2) e 
	edge g (n1, n2)			= edge' g (n1, n2)
	insertNode g n 			= insertNode' g n
	nodes g 				= nodes' g
	empty 					= Empty

maxNode :: (AdjacencyMatrix e) -> Node
maxNode (AM arr) = fst $ snd $ bounds arr 
maxNode Empty = 0

nodes' :: (AdjacencyMatrix e) -> [Node]
nodes' (AM arr) = [1 .. maxNode (AM arr)]
nodes' Empty = []

edge' :: (AdjacencyMatrix e) -> (Node, Node) -> Maybe e
edge' Empty (_,_) = Nothing
edge' (AM arr) (n1,n2) = getE (arr ! (n1,n2)) where
		 getE NoEdge = Nothing
		 getE (Ed e) = Just e

insertNode' :: (AdjacencyMatrix e) -> Node -> (AdjacencyMatrix e)
insertNode' Empty n = AM (listArray ((1,1), (n,n)) $ repeat NoEdge)
insertNode' (AM arr) n = AM (listArray ((1,1), (m,m)) $ ((elems arr) ++ (repeat NoEdge))) 
	where m = max n $ maxNode (AM arr) 

insertEdge' :: (AdjacencyMatrix e) -> (Node, Node) -> e -> (AdjacencyMatrix e)
insertEdge' (AM arr) (n1,n2) e = AM $ arr // [((n1,n2), (Ed e))]
insertEdge' Empty (_,_) _ = Empty

