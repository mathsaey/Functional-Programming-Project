-- TreeSearch.hs
-- Mathijs Saey
-- This module contains the implementation of tree based graph algorithms

module Graph.TreeSearch (findPaths) where

import Graph.Kernel
import Data.Maybe
import Data.List

--------------------------------
-- Graph Tree creation & type --
--------------------------------

data (Tree n) = Leaf n | Branch n [Tree n] deriving (Show)

-- Creates a tree from the graph
createTree ::  (Graph g n e, Eq n, Eq e) => g -> n -> (Tree n)
createTree g n = createTree' g [] n (getNeighbours g n)

-- If a node doesn't have any (unvisited) neightbours, return a leaf
-- Otherwise, create a branch with every neighbour as a child
-- make sure we don't look for neighbours of nodes that were already visited
createTree':: (Graph g n e, Eq n, Eq e) => g -> [n] -> n -> [n] -> (Tree n)
createTree' graph visited n [] = (Leaf n)
createTree' graph visited node neighbours  
	| elem node visited = (Leaf node)
	| otherwise = Branch node $ 
					map (\n -> createTree' graph newVisited n (getNeighbours graph n)) 
					neighbours where
						newVisited = node:visited

---------------------
-- Tree algorithms --
---------------------

-- Find all the possible paths between 2 nodes by traversing the tree
findPaths ::  (Graph g n e, Eq n, Eq e) => g -> n -> n -> [[n]]
findPaths g f t = findPaths' (createTree g f) t [] []

-- Always keep track of all the paths that were already found
-- keep track of how we reached this node as well
-- if we reach our goal, add the path to the paths that were already found
-- If we reach a leaf that is not our goal, just return the results
findPaths' ::  (Eq n) => (Tree n) -> n -> [n] -> [[n]] -> [[n]]
findPaths' (Branch n t) to ls res
	| n == to = [(ls ++ [n])] ++ res
	| otherwise = foldl (\res' x ->  paths x res') res t 
		where 
			paths x res = findPaths' x to curPath res
			curPath = ls ++ [n]
findPaths' (Leaf n) to ls res
	| n == to = [(ls ++ [n])] ++ res
	| otherwise = res