-- Algorithms.hs
-- Mathijs Saey
-- This module contains the implementation of the graph algorithms

module Graph.Algorithms (dijkstra, layout) where

import Graph.Kernel
import Data.Array

import Graph.BinarySearchTree

dijkstra :: (Num e, Graph g n e) => g -> n -> n -> Maybe [n]
dijkstra empty source end = Nothing

layout :: Int -> Int
layout n = n



relax :: (Num a, Ord a, Ix n) => (Array n a) -> (Array n n) -> n -> n -> a -> (Array n a, Array n n)
relax dist htr from to weight = 
	if newDist < dist ! to
		then (dist // [(to, newDist)], htr // [(to, from)])
		else (dist, htr)
		where newDist = (dist ! from) + weight

