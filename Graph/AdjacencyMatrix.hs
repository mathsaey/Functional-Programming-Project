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
	where m = max n (fst $ snd $ bounds arr)  

-------------------------
-- Read implementation --
-------------------------

instance (Read e, Show e) => Read (AdjacencyMatrix e) where
	readsPrec _ s = readGraph s

readGraph :: (Read e, Show e) => String -> [((AdjacencyMatrix e), String)]
readGraph "" = [(empty, "")]
readGraph s = [(graph, "")] where
	lns = lines s
	nds = length lns
	nodesGraph = foldl (\acc x -> insertNode acc x) empty [1..nds]
	graph = fst $ (foldl (\(g, ctr) x -> (readLine g x ctr, ctr + 1)) (nodesGraph, 1) $ lines s)

readLine :: (Read e, Show e) => (AdjacencyMatrix e) -> String -> Int -> (AdjacencyMatrix e)
readLine graph s curNode =  fst $ foldl (\(g, ctr) x -> 
	if x == "-" 
		then (g, ctr + 1) 
		else (insertEdge g (curNode, ctr) (read x), ctr + 1))
	(graph, 1)
	(words s)