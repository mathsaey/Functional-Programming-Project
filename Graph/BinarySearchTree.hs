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

insertBST :: Ord k => (BinarySearchTree k v) -> k -> v -> (BinarySearchTree k v)
insertBST EmptyTree k v = BST k v EmptyTree EmptyTree
insertBST (BST k' v' l r) k v
	| k == k' = BST k v l r
	| k < k' = BST k' v' (insertBST l k v) r
	| k > k' = BST k' v' l (insertBST r k v)

isElement :: Ord k => (BinarySearchTree k v) -> k -> (Maybe v)
isElement EmptyTree k = Nothing
isElement (BST k' v l r) k 
	| k == k' = Just v
	| k < k' = isElement l k
	| k > k' = isElement r k

--------------------------
-- Graph implementation --
--------------------------

instance (Ord e, Ord n) => Graph (BSTGraph n e) n e where
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
	| otherwise =  Graph (insertBST t (n1, n2) e) l

edge' :: (Ord n) => (BSTGraph n e) -> (n,n) -> (Maybe e)
edge' EmptyGraph _ = Nothing
edge' (Graph t _) p = isElement t p

insertNode' :: (BSTGraph n e) -> n -> (BSTGraph n e)
insertNode' EmptyGraph n = Graph EmptyTree [n]
insertNode' (Graph t l) n = Graph t $ l ++ [n]  

nodes' :: (BSTGraph n e) -> [n]
nodes' EmptyGraph = []
nodes' (Graph t l) = l

