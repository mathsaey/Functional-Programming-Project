{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- AdjacencyMatrix.hs
-- Mathijs Saey
-- This module contains the AdjacenyMatrix graph implementation

module Graph.AdjacencyMatrix (AdjacencyMatrix, insertEdge, insertNode, nodes, edge, empty) where

import Graph.Kernel
import Data.Array
import Data.List

type Node = Int
data Edge e = NoEdge | Ed e deriving (Read, Show)
data AdjacencyMatrix e = Empty | AM (Array (Node, Node) (Edge e)) [Node] deriving Show

instance (Read e) => Graph (AdjacencyMatrix e) Node e where
	insertEdge g (n1 ,n2) e = insertEdge' g (n1,n2) e 
	edge g (n1, n2)			= edge' g (n1, n2)
	insertNode g n 			= insertNode' g n
	nodes g 				= nodes' g
	empty 					= Empty

maxNode :: (AdjacencyMatrix e) -> Node
maxNode (AM arr _) = fst $ snd $ bounds arr 
maxNode Empty = 0

nodes' :: (AdjacencyMatrix e) -> [Node]
nodes' (AM arr l) = l
nodes' Empty = []

edge' :: (AdjacencyMatrix e) -> (Node, Node) -> Maybe e
edge' Empty (_,_) = Nothing
edge' (AM arr l) (n1,n2) 
	| notElem n1 l = Nothing
	| notElem n2 l = Nothing
	| otherwise =  getE $ arr ! (n1,n2) where
		 getE NoEdge = Nothing
		 getE (Ed e) = Just e

insertEdge' :: (AdjacencyMatrix e) -> (Node, Node) -> e -> (AdjacencyMatrix e)
insertEdge' Empty (_,_) _ = Empty
insertEdge' (AM arr l) (n1,n2) e 
	| notElem n1 l = AM arr l
	| notElem n2 l = AM arr l
	| otherwise =  AM (arr // [((n1,n2), (Ed e))]) l

insertNode' :: (AdjacencyMatrix e) -> Node -> (AdjacencyMatrix e)
insertNode' Empty n = AM ((listArray ((1,1), (n,n)) $ repeat NoEdge)) [n]
insertNode' (AM arr l) n = AM ((listArray ((1,1), (m,m)) ((elems arr) ++ (repeat NoEdge)))) (l ++ [n])
	where m = max n $ maxNode (AM arr l) 




--instance (Read e) => Read (AdjacencyMatrix e) where
--	readsPrec _ s = [(readsGraph empty (1,1) s, "")]

--readsGraph :: (Read e) => (AdjacencyMatrix e) -> (Node, Node) -> String -> (AdjacencyMatrix e)
--readsGraph g (n1, n2) "\0"			= g
--readsGraph g (n1, n2) (' ':xs)		= readsGraph g (n1, n2) xs
--readsGraph g (n1, n2) ('\n':xs)		= readsGraph g (n1 + 1, 1) xs
--readsGraph g (n1, n2) ('-':xs)		= readsGraph (insertNode' g n2) (n1, n2 + 1) xs
--readsGraph g (n1, n2) (x:xs)		= 
--	do ng1 <- insertNode' g n2
--	   ng2 <- insertEdge' g (n1,n2) $ read [x]
--	   readsGraph ng2 (n1, n2 + 1) xs