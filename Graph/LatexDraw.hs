-- LatexDraw.hs
-- Mathijs Saey
-- This module contains the LatexDraw implementation

module Graph.LatexDraw (toPsP) where

import Data.List
import Data.Maybe

import Text.Printf
import System.Random

import Graph.Kernel
import Graph.Layout
import Graph.Dijkstra

nodeRadius = 0.5

drawNode :: (n, Float, Float) -> String
drawNode (_, x, y) = 
	printf "\\pscircle[linecolor = black](%.2f,%.2f){%.2f}" x y nodeRadius

drawEdge :: (n, Float, Float) -> (n, Float, Float) -> String -> String
drawEdge (_, fromX, fromY) (_, toX, toY) c = 
	printf ("\\psline[linecolor =" ++ c ++ "]{->}(%.2f,%.2f)(%.2f,%.2f)") fromX fromY toX toY

toPsP :: (Num e, Graph g n e, RandomGen r, Ord e, Eq n) => g -> r -> n -> n -> String
toPsP graph gen from to = nodes ++ pathedgeStr ++ noPathedgeStr where
		layoutL = layout graph gen
		nodes = foldl (\acc x -> acc ++ (drawNode x) ++ "\n") "" layoutL
		path = dijkstra graph from to
		pathFrom = if path /= Nothing then fromJust path else []
		pathNodes = map (\x -> fromJust (find (\(n,_,_) -> n == x) layoutL)) pathFrom
		pathEdges = fst $ foldl (\(ls, e) x -> if e == x then (ls, x) else ((e,x):ls, x)) ([], head pathNodes) pathNodes
		pathedgeStr = foldl (\acc (a,b) -> acc ++ (drawEdge a b "red" ++ "\n")) "" pathEdges
		alledges = 
			foldl (\ls (n1,x,y) -> ls ++
				(foldl (\ls (a,b,c) -> ((n1,x,y), (a,b,c)) : ls) 
					[] $ (filter (\(n2,_,_) -> edge graph (n1,n2) /= Nothing) layoutL))) 
			[] layoutL
		noPathEdges = alledges \\ pathEdges
		noPathedgeStr = foldl (\acc (a,b) -> acc ++ (drawEdge a b "black" ++ "\n")) "" noPathEdges
