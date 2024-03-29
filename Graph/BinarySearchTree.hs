{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- BinarySearchTree.hs
-- Mathijs Saey
-- This module contains the Binary Search Tree graph implementation

module Graph.BinarySearchTree (BSTGraph, insertEdge, insertNode, nodes, edge, empty) where

import Graph.Kernel

data BinarySearchTree k v = EmptyTree | BST k v (BinarySearchTree k v) (BinarySearchTree k v) deriving Show
data BSTGraph n e = EmptyGraph | Graph (BinarySearchTree (n,n) e) [n] deriving Show

-------------------------
-- Tree implementation --
-------------------------

insertElement :: Ord k => (BinarySearchTree k v) -> k -> v -> (BinarySearchTree k v)
insertElement EmptyTree k v = BST k v EmptyTree EmptyTree
insertElement (BST k' v' l r) k v
	| k == k' = BST k v l r
	| k < k' = BST k' v' (insertElement l k v) r
	| k > k' = BST k' v' l (insertElement r k v)

getElement :: Ord k => (BinarySearchTree k v) -> k -> (Maybe v)
getElement EmptyTree k = Nothing
getElement (BST k' v l r) k 
	| k == k' = Just v
	| k < k' = getElement l k
	| k > k' = getElement r k

--------------------------
-- Graph implementation --
--------------------------

instance (Ord n) => Graph (BSTGraph n e) n e where
	insertEdge g (n1 ,n2) e = insertEdge' g (n1,n2) e 
	edge g (n1, n2)			= edge' g (n1, n2)
	insertNode g n 			= insertNode' g n
	nodes g 				= nodes' g
	empty 					= EmptyGraph

insertEdge' :: (Ord n) => (BSTGraph n e) -> (n,n) -> e -> (BSTGraph n e)
insertEdge' EmptyGraph (_,_) _ =  EmptyGraph
insertEdge' (Graph t l) (n1,n2) e 
	| notElem n1 l = Graph t l
	| notElem n2 l = Graph t l
	| otherwise =  Graph (insertElement t (n1, n2) e) l

edge' :: (Ord n) => (BSTGraph n e) -> (n,n) -> (Maybe e)
edge' EmptyGraph _ = Nothing
edge' (Graph t _) p = getElement t p

insertNode' :: (Eq n) => (BSTGraph n e) -> n -> (BSTGraph n e)
insertNode' EmptyGraph n = Graph EmptyTree [n]
insertNode' (Graph t l) n 
	| elem n l = (Graph t l)
	| otherwise =  Graph t $ l ++ [n]  

nodes' :: (BSTGraph n e) -> [n]
nodes' EmptyGraph = []
nodes' (Graph t l) = l

-------------------------
-- Read implementation --
-------------------------

instance (Read n, Read e, Ord n) => Read (BSTGraph n e) where
	readsPrec _ s = readGraph s

readGraph :: (Read n, Read e, Ord n) => String -> [((BSTGraph n e), String)]
readGraph "" = [(empty, "")]
readGraph s = [(foldl (\acc x -> readLine acc x) empty $ lines s, "")]

readLine :: (Read n, Read e, Ord n) => (BSTGraph n e) -> String -> (BSTGraph n e)
readLine graph s = rgraph where
	wrd = words s
	nodes = read $ head wrd
	edgeR = read $ head $ tail wrd
	ngraph1 = insertNode graph $ fst nodes
	ngraph2 = insertNode ngraph1 $ snd nodes
	rgraph = insertEdge ngraph2 nodes edgeR