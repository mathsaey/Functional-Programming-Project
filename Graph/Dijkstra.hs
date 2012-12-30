-- Dijkstra.hs
-- Mathijs Saey
-- This module contains the implementation of the dijkstra algorithm

module Graph.Dijkstra (dijkstra) where

import Graph.Kernel
import Data.Maybe
import Data.List
import Infinity

front :: (a,b,c) -> a
front (x,_,_) = x 

center :: (a,b,c) -> b
center (_,x,_) = x 

back :: (a,b,c) -> c
back (_,_,x) = x 

relax :: (Num e, Ord e) => (n,n,(Inf e)) -> n -> (Inf e) -> (Inf e) -> (n,n,(Inf e))
relax (to,_, INF) newFrom (NI fromW) (NI edgeW) = (to, newFrom, NI $ fromW + edgeW)
relax (to, oldFrom, oldWeight) newFrom INF (NI edgeW) = (to, oldFrom , oldWeight)
relax (to, oldFrom, (NI oldWeight)) newFrom (NI fromW) (NI edgeW)
	| newWeight < oldWeight = (to, newFrom, NI newWeight)
	| otherwise = (to, oldFrom, NI oldWeight)
		where newWeight = edgeW + fromW

relaxNeighbours ::  (Graph g n e, Num e, Ord e, Eq n) => g -> n -> [(n,n, (Inf e))] -> [(n, n, (Inf e))]
relaxNeighbours graph source stateList = 
	map (\(to,from, weight) -> if elem to neighbours 
		then relax (to, from, weight) source sourceW $ NI $ fromJust (edge graph (source, to))
		else (to, from, weight))
	stateList
	where 
		neighbours = getNeighbours graph source
		sourceW = back $ fromJust (find (\(to,_, weight) -> to == source) stateList)

dijkstraLoop :: (Graph g n e, Num e, Ord e, Eq n) => g -> [(n,n, (Inf e))] -> [n] -> [n] -> n -> [(n,n, (Inf e))]
dijkstraLoop _ s _ [] _ = s
dijkstraLoop graph stateList visited unvisited current = 
	dijkstraLoop graph newState (current:visited) (delete (front smallest) unvisited) (front smallest)
	where 
		newState = relaxNeighbours graph current stateList
		smallest = foldl accFunc (head unvisited, head unvisited, INF) newState 
		accFunc (to', from', weight') (to, from, weight)  
			| notElem to unvisited = (to', from', weight')
			| weight > weight' = (to', from', weight')
			| otherwise = (to, from, weight)

getPath ::  (Eq n, Eq e) => [(n,n, (Inf e))] -> n -> n -> [n] -> Maybe [n]
getPath state dest curr lst
	| dest == curr = Just $ dest:lst
	| weight == INF = Nothing
	| otherwise =  getPath state dest from (node:lst)
		where 
			node = front triple
			from = center triple
			weight = back triple
			triple = fromJust $ find (\(node, from, weight) -> node == curr) state

dijkstra :: (Num e, Eq e, Eq n, Ord e, Graph g n e) => g -> n -> n -> Maybe [n]
dijkstra graph source end
	| nodes graph == [] = Nothing
	| notElem end $ nodes graph = Nothing
	| notElem source $ nodes graph = Nothing	
	| otherwise = getPath (dijkstraLoop
		graph
		(map (\x -> if x == source then (x, x, NI 0) else (x, x, INF)) (nodes graph))
		[]
		(delete source (nodes graph))
		source)
	source end []