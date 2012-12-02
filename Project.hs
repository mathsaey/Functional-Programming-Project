{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,FlexibleInstances, FlexibleContexts #-}

import Text.Printf

-- DEBUG
import Graph.BinarySearchTree
import Graph.Layout
import System.Random


t0 = foldl (\acc x -> insertNode acc x) (empty :: (BSTGraph Int Int)) [1..5]
t1 = insertEdge t0 (1,2) 1
t2 = insertEdge t1 (2,3) 1
t3 = insertEdge t2 (3,4) 1
t4 = insertEdge t3 (4,5) 1
t5 = insertEdge t4 (1,5) 2

x = t4
y = t5

gen = mkStdGen 2435
-- END DEBUG

nodeRadius = 0.5

drawNode :: (n, Float, Float) -> String
drawNode (_, x, y) = 
	printf "\\pscircle[linecolor = black](%.2f,%.2f){%.2f}" x y nodeRadius

drawEdge :: (n, Float, Float) -> (n, Float, Float) -> String
drawEdge (_, fromX, fromY) (_, toX, toY) = 
	printf "\\psline[linecolor = black]{->}(,)(,)" fromX fromY toX toY

drawGraph :: [(n, Float, Float)] -> String
drawGraph [] = ""
drawGraph ls = foldl (\acc x -> acc ++ (drawNode x) ++ "\n") "" ls

