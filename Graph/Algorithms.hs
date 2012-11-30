-- Algorithms.hs
-- Mathijs Saey
-- This module contains the implementation of the graph algorithms

module Graph.Algorithms (dijkstra, layout) where

import Graph.Kernel
import Data.Array
import Data.Maybe

import Graph.BinarySearchTree

--dijkstra :: (Num e, Graph g n e) => g -> n -> n -> Maybe [n]
--dijkstra empty source end = Nothing
dijkstra graph source end = dijkstraLoop 
	graph
	end
	(listArray (minNode, maxNode) (map (\x -> if x == source then 0 else read "Infinity") $ nodes graph))
	(listArray (minNode, maxNode) $ nodes graph) 
	[source]
	[]
	where 
		minNode = minimum $ nodes graph
		maxNode = maximum $ nodes graph

getNeighbours :: (Eq e, Eq n) => (Graph g n e) => g -> n -> [n]
getNeighbours g n = [i | i <- nodes g, i /= n, edge g (n, i) /= Nothing]

-- See if it's faster to reach to via (from, to) than via the previous route.
relax :: (Num e, Ord e, Ix n) => (Array n e) -> (Array n n) -> n -> n -> e -> (Array n e, Array n n)
relax dist htr from to weight = 
	if newDist < dist ! to
		then (dist // [(to, newDist)], htr // [(to, from)])
		else (dist, htr)
		where newDist = (dist ! from) + weight

-- Relax all the neighbours of a certain node
relaxNeighbours :: (Graph g n e, Num e, Ord e, Ix n) => g -> n -> [n] -> (Array n e) -> (Array n n) -> (Array n e, Array n n)
relaxNeighbours graph from [] dist htr = (dist, htr)
relaxNeighbours graph from (x:xs) dist htr = relaxNeighbours graph from xs (fst relaxed) (snd relaxed)
	where relaxed = relax dist htr from  x $ (fromJust $ edge graph (from, x)) 

--dijkstraLoop :: (Graph g n e, Ix n, Ord e, Eq n, Eq e, Num e, Read e) => g -> n -> (Array n e) -> (Array n n) -> [n] -> [n] -> (Array n n)
dijkstraLoop graph end dist htr visited [] = (dist, htr)
dijkstraLoop graph end dist htr (x:xs) un 
	| x == end = (fst relaxed, snd relaxed)
	| (snd minDist) == (read "Infinity") = (fst relaxed, snd relaxed)
	| otherwise =  dijkstraLoop graph end (fst relaxed) (snd relaxed) ((fst $ minDist):(x:xs)) (filter (\x -> x /= (fst $ minDist)) un)
	where 
		relaxed = relaxNeighbours graph x (getNeighbours graph x) dist htr
		minDist = foldl 
					(\(node, dist) (accNode, accDist) -> if dist < accDist then (node, dist) else (accNode, accDist)) 
					(fst $ head $ assocs dist, snd $ head $ assocs dist) 
					$ assocs dist




layout :: Int -> Int
layout n = n
