{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- AdjacencyMatrix.hs
-- Mathijs Saey
-- This module contains the AdjacenyMatrix graph implementation

module Graph.AdjacencyMatrix (AdjacencyMatrix, insertEdge, insertNode, nodes, edge, empty) where

import Graph.Kernel hiding (Node, No)
import Data.Array
import Data.List

type Node = Int
data AdjacencyMatrix e = Empty | AM (Array (Int, Int) e) deriving Show

instance (Read e) => Graph (AdjacencyMatrix e) Node e where
	insertEdge g (n1 ,n2) e = insertEdge' g (n1,n2) e 
	--edge g (n1, n2)			= edge' g (n1, n2)
	insertNode g n 			= insertNode' g n
	nodes g 				= nodes' g
	empty 					= Empty

-- makeGraph :: Int -> AdjacencyMatrix
-- makeGraph n = AM $ listArray ((1, n), (1,n)) [1..n]
-- vector ! idx => element

-- Work functions
maxNode :: (AdjacencyMatrix e) -> Node
maxNode Empty = 0
maxNode (AM arr) = fst $ snd $ bounds arr 

-- Instance functions

insertNode' :: (AdjacencyMatrix e) -> Int -> (AdjacencyMatrix e)
insertNode' (AM arr) n = AM (listArray ((1,1), (x,x)) (elems arr)) where x = maxNode (AM arr) 
insertNode' Empty n = AM (listArray ((1,1), (n,n)) [])

insertEdge' :: (AdjacencyMatrix e) -> (Node, Node) -> e -> (AdjacencyMatrix e)
insertEdge' (AM arr) (n1,n2) e = AM $ arr // [((n1,n2), e)]
insertEdge' Empty (_,_) _ = Empty

nodes' :: (AdjacencyMatrix e) -> [Node]
nodes' (AM arr) = [1 .. maxNode (AM arr)]
nodes' Empty = []


--edge' :: (AdjacencyMatrix e) -> (Node, Node) -> Maybe (Edge e)
--edge' Empty (_,_) = Nothing

-- ding x where
	-- ding 0 = sdlfj
	-- ding n = 